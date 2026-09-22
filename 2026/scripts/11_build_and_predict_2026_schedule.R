library(dplyr)
library(lubridate)
library(catboost)

# ════════════════════════════════════════════════════════════════════════════════
#  BUILD + PREDICT REMAINING 2026 SCHEDULE
#
#  Because the 2026 second half hasn't been played yet, model_features.csv
#  has no post-deadline rows for Season 2026 (01_prepare_and_engineer only
#  builds a row once a game's actual result exists). This script builds the
#  equivalent feature rows directly from the remaining schedule instead:
#
#    1. Compute each team's pre-deadline (actual, through the trade deadline) rolling/
#       cumulative/trend features from real 2026 first-half data — same
#       feature engineering as 01_prepare_and_engineer, restricted to 2026.
#    2. Attach those features (own + opponent) to every scheduled remaining
#       game from schedule_2026_remaining.csv (built by
#       00_scrape_2026_schedule_finished.R).
#    3. Run the actual trained CatBoost models on those rows to get real
#       per-game predictions for the rest of the season.
#    4. Aggregate to full-season team projections (actual first half +
#       projected rest of season).
#
#  Output:
#    Outputs\schedule_predictions_2026_game_level.csv
#    Outputs\schedule_predictions_2026_team_summary.csv
#
#  These are intentionally separate from predictions_2026_*.csv, which stay
#  reserved for after-the-fact evaluation once real second-half results
#  exist (feeding the Awaiting 2026 Second Half Data scripts).
# ════════════════════════════════════════════════════════════════════════════════

DATA_DIR   <- "../data/"
MODELS_DIR <- "../models/"
OUT_DIR    <- "../outputs/"
SEASON     <- 2026
DEADLINE   <- as.Date(sprintf("%d-08-03", SEASON))  # 2026 trade deadline moved to Aug 3

# ── 1. LOAD RAW FIRST-HALF DATA ────────────────────────────────────────────────
df_raw <- read.csv(paste0(DATA_DIR, "stathead_team.csv"), stringsAsFactors = FALSE)

df <- df_raw %>%
  select(
    Team, Date, Opp,
    home_away    = `...5`,
    Result,
    bat_PA       = PA,
    bat_AB       = AB,
    bat_R        = R,
    bat_H        = H,
    bat_1B       = X1B,
    bat_2B       = X2B,
    bat_3B       = X3B,
    bat_HR       = HR,
    bat_RBI      = RBI,
    bat_SB       = SB,
    bat_CS       = CS,
    bat_BB       = BB,
    bat_SO       = SO,
    bat_BA       = BA,
    bat_OBP      = OBP,
    bat_SLG      = SLG,
    bat_OPS      = OPS,
    bat_TB       = TB,
    bat_GIDP     = GIDP,
    bat_HBP      = HBP,
    bat_SH       = SH,
    bat_SF       = SF,
    bat_IBB      = IBB,
    bat_XBH      = XBH,
    bat_TOB      = TOB,
    bat_TOBwe    = TOBwe,
    bat_ROE      = ROE,
    bat_WPA      = `WPA...4`,
    bat_RE24     = RE24,
    bat_aLI      = aLI,
    bat_LOB      = LOB,
    pit_IP       = IP,
    pit_H        = H_pit,
    pit_R        = R_pit,
    pit_ER       = ER,
    pit_UER      = UER,
    pit_HR       = HR_pit,
    pit_BB       = BB_pit,
    pit_IBB      = IBB_pit,
    pit_SO       = SO_pit,
    pit_HBP      = HBP_pit,
    pit_BK       = BK,
    pit_WP       = WP,
    pit_BF       = BF,
    pit_BR       = BR,
    pit_WPA      = WPA,
    Season       = scrape_year
  ) %>%
  filter(Season == SEASON)

df$Date <- as.Date(df$Date)

df$WL         <- substr(df$Result, 1, 1)
scores        <- regmatches(df$Result, regexpr("[0-9]+-[0-9]+", df$Result))
score_split   <- strsplit(scores, "-")
df$team_score <- as.integer(sapply(score_split, `[`, 1))
df$opp_score  <- as.integer(sapply(score_split, `[`, 2))
df$win        <- as.integer(df$WL == "W")
df$Result     <- NULL
df$WL         <- NULL

df$is_home    <- as.integer(is.na(df$home_away))
df$home_away  <- NULL

df$Team <- dplyr::recode(df$Team, "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
df$Opp  <- dplyr::recode(df$Opp,  "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")

df$deadline_date    <- DEADLINE
df$is_post_deadline <- as.integer(df$Date > df$deadline_date)

df$pit_IP_numeric <- as.numeric(df$pit_IP)
df$game_ERA <- ifelse(df$pit_IP_numeric > 0,
                      (df$pit_ER / df$pit_IP_numeric) * 9,
                      NA_real_)

df <- df %>% filter(is_post_deadline == 0) %>% arrange(Team, Date)

cat(sprintf("Loaded %d actual 2026 pre-deadline games across %d teams\n",
            nrow(df), length(unique(df$Team))))

# ── HELPER FUNCTIONS (identical to 01_prepare_and_engineer) ───────────────────
wls_slope <- function(y) {
  n <- length(y)
  if (n < 3) return(NA_real_)
  x       <- seq_len(n)
  weights <- exp(seq(0, 1, length.out = n))
  tryCatch({
    coef(lm(y ~ x, weights = weights))[["x"]]
  }, error = function(e) NA_real_)
}
safe_mean <- function(x) mean(x, na.rm = TRUE)
safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x,   na.rm = TRUE)
  if (is.na(m) || m == 0) return(NA_real_)
  s / m
}

# ── 2. PRE-DEADLINE FEATURES (own) — same aggregation as 01 ───────────────────
pre <- df %>%
  arrange(Team, Date) %>%
  group_by(Team, Season) %>%
  summarise(
    n_pre_games = n(),
    cum_R_per_game    = safe_mean(bat_R),
    cum_H_per_game    = safe_mean(bat_H),
    cum_HR_per_game   = safe_mean(bat_HR),
    cum_BB_per_game   = safe_mean(bat_BB),
    cum_SO_per_game   = safe_mean(bat_SO),
    cum_RBI_per_game  = safe_mean(bat_RBI),
    cum_SB_per_game   = safe_mean(bat_SB),
    cum_LOB_per_game  = safe_mean(bat_LOB),
    cum_OBP           = safe_mean(bat_OBP),
    cum_SLG           = safe_mean(bat_SLG),
    cum_OPS           = safe_mean(bat_OPS),
    cum_BA            = safe_mean(bat_BA),
    cum_XBH_per_game  = safe_mean(bat_XBH),
    cum_WPA_bat       = safe_mean(bat_WPA),
    cum_RE24_bat      = safe_mean(bat_RE24),
    cum_RA_per_game   = safe_mean(opp_score),
    cum_ER_per_game   = safe_mean(pit_ER),
    cum_H_allowed_pg  = safe_mean(pit_H),
    cum_HR_allowed_pg = safe_mean(pit_HR),
    cum_BB_allowed_pg = safe_mean(pit_BB),
    cum_SO_pit_pg     = safe_mean(pit_SO),
    cum_HBP_pit_pg    = safe_mean(pit_HBP),
    cum_WP_per_game   = safe_mean(pit_WP),
    cum_ERA           = safe_mean(game_ERA),
    cum_win_rate      = safe_mean(win),
    cum_run_diff_pg   = safe_mean(bat_R - opp_score),

    cv_run_diff       = safe_cv(abs(bat_R - opp_score)),

    q25_R_per_game    = quantile(bat_R,     0.25, na.rm = TRUE),
    q75_R_per_game    = quantile(bat_R,     0.75, na.rm = TRUE),
    iqr_R_per_game    = quantile(bat_R,     0.75, na.rm = TRUE) -
                        quantile(bat_R,     0.25, na.rm = TRUE),
    q25_RA_per_game   = quantile(opp_score, 0.25, na.rm = TRUE),
    q75_RA_per_game   = quantile(opp_score, 0.75, na.rm = TRUE),
    iqr_RA_per_game   = quantile(opp_score, 0.75, na.rm = TRUE) -
                        quantile(opp_score, 0.25, na.rm = TRUE),
    q25_run_diff      = quantile(bat_R - opp_score, 0.25, na.rm = TRUE),
    q75_run_diff      = quantile(bat_R - opp_score, 0.75, na.rm = TRUE),
    iqr_run_diff      = quantile(bat_R - opp_score, 0.75, na.rm = TRUE) -
                        quantile(bat_R - opp_score, 0.25, na.rm = TRUE),

    roll_R_per_game    = safe_mean(tail(bat_R,     pmin(n(), 15))),
    roll_RA_per_game   = safe_mean(tail(opp_score, pmin(n(), 15))),
    roll_OBP           = safe_mean(tail(bat_OBP,   pmin(n(), 15))),
    roll_SLG           = safe_mean(tail(bat_SLG,   pmin(n(), 15))),
    roll_OPS           = safe_mean(tail(bat_OPS,   pmin(n(), 15))),
    roll_HR_per_game   = safe_mean(tail(bat_HR,    pmin(n(), 15))),
    roll_BB_per_game   = safe_mean(tail(bat_BB,    pmin(n(), 15))),
    roll_SO_per_game   = safe_mean(tail(bat_SO,    pmin(n(), 15))),
    roll_ERA           = safe_mean(tail(game_ERA,  pmin(n(), 15))),
    roll_win_rate      = safe_mean(tail(win,        pmin(n(), 15))),
    roll_run_diff_pg   = safe_mean(tail(bat_R - opp_score, pmin(n(), 15))),
    roll_SO_pit_pg     = safe_mean(tail(pit_SO,    pmin(n(), 15))),
    roll_BB_allowed_pg = safe_mean(tail(pit_BB,    pmin(n(), 15))),

    slope_R_per_game   = wls_slope(bat_R),
    slope_RA_per_game  = wls_slope(opp_score),
    slope_win_rate     = wls_slope(win),
    slope_OPS          = wls_slope(bat_OPS),
    slope_ERA          = wls_slope(game_ERA),
    slope_run_diff     = wls_slope(bat_R - opp_score),

    # kept for full-season aggregation below (not fed to the model)
    actual_wins_first_half   = sum(win, na.rm = TRUE),
    actual_games_first_half  = n(),
    actual_rs_total_first_half = sum(bat_R, na.rm = TRUE),
    actual_ra_total_first_half = sum(opp_score, na.rm = TRUE),

    .groups = "drop"
  )

# ── 3. PITCHER SPLIT FEATURES (own) — same as 01, restricted to 2026 ─────────
pitcher_files <- list.files(
  path    = DATA_DIR,
  pattern = "pitcher_batch_.*\\.csv",
  full.names = TRUE
)

if (length(pitcher_files) > 0) {
  pit_raw <- bind_rows(lapply(pitcher_files, read.csv, stringsAsFactors = FALSE))
  pit_raw <- pit_raw %>% filter(scrape_year == SEASON)

  if (nrow(pit_raw) > 0) {
    pit_raw$Date   <- as.Date(gsub(" \\(.*\\)", "", pit_raw$Date))
    pit_raw$Team   <- dplyr::recode(pit_raw$Team,
                                    "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
    pit_raw$Season <- pit_raw$scrape_year
    pit_raw$deadline_date <- DEADLINE
    pit_raw <- pit_raw %>% filter(Date <= deadline_date)

    pit_raw$is_starter <- grepl("^GS", pit_raw$App.Dec)

    convert_ip <- function(ip) {
      ip <- suppressWarnings(as.numeric(ip))
      ifelse(is.na(ip), 0,
      ifelse(round(ip %% 1, 1) == 0.1, floor(ip) + 1/3,
      ifelse(round(ip %% 1, 1) == 0.2, floor(ip) + 2/3,
             floor(ip))))
    }
    pit_raw$IP_decimal <- convert_ip(pit_raw$IP)
    pit_raw$ER_num     <- suppressWarnings(as.numeric(pit_raw$ER))
    pit_raw$ER_num[is.na(pit_raw$ER_num)] <- 0

    pit_features <- pit_raw %>%
      group_by(Team, Season) %>%
      summarise(
        n_pitcher_games = n_distinct(Date),
        starter_IP_total   = sum(IP_decimal[is_starter],  na.rm = TRUE),
        starter_ER_total   = sum(ER_num[is_starter],      na.rm = TRUE),
        starter_ERA        = ifelse(starter_IP_total > 0,
                                    (starter_ER_total / starter_IP_total) * 9,
                                    NA_real_),
        starter_IP_per_game = starter_IP_total /
                              pmax(n_distinct(Date[is_starter]), 1),
        bullpen_IP_total   = sum(IP_decimal[!is_starter], na.rm = TRUE),
        bullpen_ER_total   = sum(ER_num[!is_starter],     na.rm = TRUE),
        bullpen_ERA        = ifelse(bullpen_IP_total > 0,
                                    (bullpen_ER_total / bullpen_IP_total) * 9,
                                    NA_real_),
        bullpen_IP_per_game = bullpen_IP_total /
                              pmax(n_distinct(Date[!is_starter]), 1),
        starter_pct_IP     = starter_IP_total /
                             pmax(starter_IP_total + bullpen_IP_total, 1),
        .groups = "drop"
      ) %>%
      select(-starter_ER_total, -bullpen_ER_total, -n_pitcher_games)

    pre <- pre %>% left_join(pit_features, by = c("Team", "Season"))
    cat("Pitcher split features joined.\n")
  } else {
    cat(sprintf("No individual-pitcher rows found for %d — run Batch 12 of\n", SEASON))
    cat("00_scrape_ind_pitcher_games_finished.R first if you want starter_/bullpen_ features.\n")
  }
} else {
  cat("No pitcher_batch_*.csv files found — skipping pitcher split features.\n")
}

# ── 4. OPPONENT FEATURES ───────────────────────────────────────────────────────
own_cols <- c("actual_wins_first_half", "actual_games_first_half",
              "actual_rs_total_first_half", "actual_ra_total_first_half")

opp_features <- pre %>%
  select(-all_of(own_cols)) %>%
  rename_with(~ paste0("opp_", .), -c(Team, Season)) %>%
  rename(Opp = Team)

# ── 5. LOAD REMAINING SCHEDULE AND ATTACH FEATURES ────────────────────────────
schedule <- read.csv(paste0(DATA_DIR, "schedule_2026_remaining.csv"), stringsAsFactors = FALSE)
schedule$Date   <- as.Date(schedule$Date)
schedule$Season <- SEASON

pred_input <- schedule %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(second_half_game_num = row_number()) %>%
  ungroup() %>%
  mutate(is_shortened_season = 0L) %>%
  left_join(pre %>% select(-all_of(own_cols)), by = c("Team", "Season")) %>%
  left_join(opp_features,                      by = c("Opp",  "Season"))

cat(sprintf("Built %d remaining-game feature rows across %d teams\n",
            nrow(pred_input), length(unique(pred_input$Team))))

# ── 6. LOAD TRAINED MODELS + PREDICT ──────────────────────────────────────────
model_rs <- catboost.load_model(paste0(MODELS_DIR, "model_runs_scored.cbm"))
model_ra <- catboost.load_model(paste0(MODELS_DIR, "model_runs_allowed.cbm"))
model_wl <- catboost.load_model(paste0(MODELS_DIR, "model_win_loss.cbm"))

id_cols <- c("Team", "Season", "Date", "Opp")

feat_cols_rs <- trimws(readLines(paste0(MODELS_DIR, "opt_features_runs_scored.txt")))
feat_cols_ra <- trimws(readLines(paste0(MODELS_DIR, "opt_features_runs_allowed.txt")))
feat_cols_wl <- trimws(readLines(paste0(MODELS_DIR, "opt_features_win_loss.txt")))
feat_cols_rs <- feat_cols_rs[feat_cols_rs %in% names(pred_input)]
feat_cols_ra <- feat_cols_ra[feat_cols_ra %in% names(pred_input)]
feat_cols_wl <- feat_cols_wl[feat_cols_wl %in% names(pred_input)]

make_pool_with_cats <- function(data, feat_cols, cat_cols) {
  all_cols <- c(feat_cols, cat_cols)
  all_cols <- all_cols[all_cols %in% names(data)]
  X <- data[, all_cols]
  for (col in cat_cols) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X)
}

logit_to_prob <- function(x) 1 / (1 + exp(-x))

pool_rs <- make_pool_with_cats(pred_input, feat_cols_rs, c("Team", "Opp"))
pool_ra <- make_pool_with_cats(pred_input, feat_cols_ra, c("Team", "Opp"))
pool_wl <- make_pool_with_cats(pred_input, feat_cols_wl[!feat_cols_wl %in% c("Team", "Opp")], c("Team", "Opp"))

pred_input$pred_runs_scored  <- catboost.predict(model_rs, pool_rs)
pred_input$pred_runs_allowed <- catboost.predict(model_ra, pool_ra)
pred_input$pred_win_prob     <- logit_to_prob(catboost.predict(model_wl, pool_wl))
pred_input$pred_win          <- as.integer(pred_input$pred_win_prob >= 0.5)

cat(sprintf("Predicted %d remaining games.\n", nrow(pred_input)))

game_level_out <- pred_input %>%
  select(Team, Season, Date, Opp, is_home, second_half_game_num,
         pred_runs_scored, pred_runs_allowed, pred_win_prob, pred_win)

write.csv(game_level_out, paste0(OUT_DIR, "schedule_predictions_2026_game_level.csv"),
          row.names = FALSE)

# ── 7. AGGREGATE TO FULL-SEASON TEAM PROJECTIONS ──────────────────────────────
first_half <- pre %>%
  select(Team, actual_games_first_half, actual_wins_first_half,
         actual_rs_total_first_half, actual_ra_total_first_half)

Z90 <- 1.645

team_summary <- game_level_out %>%
  group_by(Team) %>%
  summarise(
    remaining_games   = n(),
    proj_wins_remaining = sum(pred_win_prob, na.rm = TRUE),
    proj_rs_remaining   = sum(pred_runs_scored, na.rm = TRUE),
    proj_ra_remaining   = sum(pred_runs_allowed, na.rm = TRUE),
    proj_wins_remaining_sd = sqrt(sum(pred_win_prob * (1 - pred_win_prob), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  left_join(first_half, by = "Team") %>%
  mutate(
    pre_win_pct        = round(actual_wins_first_half / actual_games_first_half, 3),
    proj_post_win_pct  = round(proj_wins_remaining / remaining_games, 3),
    win_pct_delta      = round(proj_post_win_pct - pre_win_pct, 3),

    # Wins must be rounded to a whole number (not 1 decimal place) here --
    # games-back math downstream (GB = ((W1-W2)+(L2-L1))/2) only produces
    # the correct whole-or-half-game values when win/loss records are whole
    # numbers, exactly like real standings. Losses are then derived from
    # games minus (rounded) wins rather than rounded independently, so
    # wins + losses always equals the (already whole) games total exactly.
    proj_full_season_games    = actual_games_first_half + remaining_games,
    proj_full_season_wins     = round(actual_wins_first_half + proj_wins_remaining),
    proj_full_season_losses   = proj_full_season_games - proj_full_season_wins,
    proj_full_season_win_pct  = round(proj_full_season_wins / proj_full_season_games, 3),
    proj_full_season_rs_total = round(actual_rs_total_first_half + proj_rs_remaining, 1),
    proj_full_season_ra_total = round(actual_ra_total_first_half + proj_ra_remaining, 1),
    proj_full_season_run_diff = round(proj_full_season_rs_total - proj_full_season_ra_total, 1),

    proj_full_season_wins_ci_lo = round(proj_full_season_wins - Z90 * proj_wins_remaining_sd),
    proj_full_season_wins_ci_hi = round(proj_full_season_wins + Z90 * proj_wins_remaining_sd)
  ) %>%
  arrange(desc(proj_full_season_wins))

write.csv(team_summary, paste0(OUT_DIR, "schedule_predictions_2026_team_summary.csv"),
          row.names = FALSE)

cat("\nSaved schedule_predictions_2026_game_level.csv and schedule_predictions_2026_team_summary.csv\n")
cat("Next: run 12_team_projection_panels, 13_playoff_projections, 14_win_pct_comparison\n")
