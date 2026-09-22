library(dplyr)
library(lubridate)

# ════════════════════════════════════════════════════════════════════════════════
#  SCRIPT 1 OF 3 — DATA CLEANING & FEATURE ENGINEERING
#  Input:  stathead_team.csv
#  Output: model_features.csv
#
#  What this script does:
#    1. Loads raw StatHead combined batting + pitching data
#    2. Cleans columns, parses results, unifies franchise identifiers
#    3. Splits each season at its trade deadline (July 31 through 2025; August 3 for 2026)
#    4. Engineers pre-deadline features per team-season:
#         - Cumulative season-to-date averages
#         - 15-game rolling averages
#         - Weighted least squares trend slopes
#         - Variance / consistency features (cv_run_diff)
#    5. Adds post-deadline game context features:
#         - second_half_game_num
#         - is_home, is_shortened_season
#    6. Mirrors all team features for the opponent
#    7. Outputs one row per post-deadline game, ready for modeling
# ════════════════════════════════════════════════════════════════════════════════

# ── 0. LOAD ────────────────────────────────────────────────────────────────────
df_raw <- read.csv("../data/stathead_team.csv", stringsAsFactors = FALSE)

# ── 1. SELECT AND RENAME COLUMNS ──────────────────────────────────────────────
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
  )


# ── 2. PARSE DATE ──────────────────────────────────────────────────────────────
df$Date <- as.Date(df$Date)

# 3. PARSE RESULT
df$WL         <- substr(df$Result, 1, 1)
scores        <- regmatches(df$Result, regexpr("[0-9]+-[0-9]+", df$Result))
score_split   <- strsplit(scores, "-")
df$team_score <- as.integer(sapply(score_split, `[`, 1))
df$opp_score  <- as.integer(sapply(score_split, `[`, 2))
df$win        <- as.integer(df$WL == "W")
df$Result     <- NULL
df$WL         <- NULL

#4. RECODE HOME/AWAY
df$is_home    <- as.integer(is.na(df$home_away))
df$home_away  <- NULL

#5. UNIFY FRANCHISE IDENTIFIERS
df$Team <- dplyr::recode(df$Team, "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
df$Opp  <- dplyr::recode(df$Opp,  "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
cat("Unique teams after unification:", length(unique(df$Team)), "\n")

# ── 6. FLAGS ──────────────────────────────────────────────────────────────────
df$is_shortened_season <- as.integer(df$Season == 2020)

# ── 7. DEADLINE SPLIT ─────────────────────────────────────────────────────────
#  Trade deadline was July 31 through 2025; 2026 moved it to August 3.
df$deadline_date    <- as.Date(ifelse(df$Season == 2026,
                                      paste0(df$Season, "-08-03"),
                                      paste0(df$Season, "-07-31")))
df$is_post_deadline <- as.integer(df$Date > df$deadline_date)

# ── 8. GAME-LEVEL ERA ─────────────────────────────────────────────────────────
df$pit_IP_numeric <- as.numeric(df$pit_IP)
df$game_ERA <- ifelse(df$pit_IP_numeric > 0,
                      (df$pit_ER / df$pit_IP_numeric) * 9,
                      NA_real_)

# ── 9. SORT ───────────────────────────────────────────────────────────────────
df <- df %>% arrange(Team, Season, Date)

cat("\nSanity checks:\n")
cat("  Win rate (should be ~0.50):", round(mean(df$win, na.rm = TRUE), 3), "\n")
cat("  Avg runs scored:           ", round(mean(df$team_score, na.rm = TRUE), 2), "\n")
cat("  Avg runs allowed:          ", round(mean(df$opp_score,  na.rm = TRUE), 2), "\n")

#  FEATURE ENGINEERING

# ── HELPER FUNCTIONS ──────────────────────────────────────────────────────────

# Weighted Least Squares slope — recent games weighted more heavily
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

# Coefficient of variation — volatility relative to mean
safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x,   na.rm = TRUE)
  if (is.na(m) || m == 0) return(NA_real_)
  s / m
}

# ── PRE-DEADLINE FEATURES ─────────────────────────────────────────────────────

pre <- df %>%
  filter(is_post_deadline == 0) %>%
  arrange(Team, Season, Date) %>%
  group_by(Team, Season) %>%
  summarise(

    n_pre_games = n(),

    # ── Cumulative season-to-date averages ────────────────────────────────────
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

    # ── Variance / consistency (cv_run_diff showed meaningful importance) ─────
    cv_run_diff       = safe_cv(abs(bat_R - opp_score)),

    # ── Quartile features — team-specific scoring distribution ────────────────
    # Captures the realistic range of outcomes for each team independently
    # of average quality — gives the model signal about how consistently
    # or erratically each team performs entering the deadline
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

    # ── Rolling 15-game averages ──────────────────────────────────────────────
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

    # ── WLS trend slopes ──────────────────────────────────────────────────────
    slope_R_per_game   = wls_slope(bat_R),
    slope_RA_per_game  = wls_slope(opp_score),
    slope_win_rate     = wls_slope(win),
    slope_OPS          = wls_slope(bat_OPS),
    slope_ERA          = wls_slope(game_ERA),
    slope_run_diff     = wls_slope(bat_R - opp_score),

    .groups = "drop"
  )


# ── PITCHER SPLIT FEATURES ────────────────────────────────────────────────────
# Loads player-level pitcher game logs (scraped separately) and computes
# starter vs bullpen ERA and IP splits per team per season through the deadline.
# These features capture pitching staff structure independently of blended
# team-level ERA, which cannot distinguish rotation-dependent from
# bullpen-dependent teams.


pitcher_files <- list.files(
  path    = "../data",
  pattern = "pitcher_batch_.*\\.csv",
  full.names = TRUE
)

if (length(pitcher_files) > 0) {

  pit_raw <- bind_rows(lapply(pitcher_files, read.csv, stringsAsFactors = FALSE))
  cat("Pitcher rows loaded:", nrow(pit_raw), "\n")

  # Clean dates — strip doubleheader annotations like (1), (2)
  pit_raw$Date   <- as.Date(gsub(" \\(.*\\)", "", pit_raw$Date))
  pit_raw$Team   <- dplyr::recode(pit_raw$Team,
                                  "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
  pit_raw$Season <- pit_raw$scrape_year

  # Filter to pre-deadline only (belt and suspenders — scrape already did this)
  pit_raw$deadline_date <- as.Date(ifelse(pit_raw$Season == 2026,
                                          paste0(pit_raw$Season, "-08-03"),
                                          paste0(pit_raw$Season, "-07-31")))
  pit_raw <- pit_raw %>% filter(Date <= deadline_date)

  # Identify starters — any appearance starting with "GS"
  pit_raw$is_starter <- grepl("^GS", pit_raw$App.Dec)

  # Convert IP to true decimal innings
  # StatHead uses .1 = 1/3 inning, .2 = 2/3 inning
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

  # Compute per-team per-season aggregates split by starter vs reliever
  pit_features <- pit_raw %>%
    group_by(Team, Season) %>%
    summarise(
      # Total games — used to compute per-game rates
      n_pitcher_games = n_distinct(Date),

      # ── Starter features ──────────────────────────────────────────────────
      starter_IP_total   = sum(IP_decimal[is_starter],  na.rm = TRUE),
      starter_ER_total   = sum(ER_num[is_starter],      na.rm = TRUE),
      starter_ERA        = ifelse(starter_IP_total > 0,
                                  (starter_ER_total / starter_IP_total) * 9,
                                  NA_real_),
      starter_IP_per_game = starter_IP_total /
                            pmax(n_distinct(Date[is_starter]), 1),

      # ── Bullpen features ──────────────────────────────────────────────────
      bullpen_IP_total   = sum(IP_decimal[!is_starter], na.rm = TRUE),
      bullpen_ER_total   = sum(ER_num[!is_starter],     na.rm = TRUE),
      bullpen_ERA        = ifelse(bullpen_IP_total > 0,
                                  (bullpen_ER_total / bullpen_IP_total) * 9,
                                  NA_real_),
      bullpen_IP_per_game = bullpen_IP_total /
                            pmax(n_distinct(Date[!is_starter]), 1),

      # ── Staff structure ───────────────────────────────────────────────────
      # What fraction of innings came from starters vs bullpen
      starter_pct_IP     = starter_IP_total /
                           pmax(starter_IP_total + bullpen_IP_total, 1),

      .groups = "drop"
    ) %>%
    select(-starter_ER_total, -bullpen_ER_total, -n_pitcher_games)

  cat("Pitcher feature rows:", nrow(pit_features), "\n")

  # Sanity check
  cat(sprintf("  Avg starter ERA:         %.2f\n",
              mean(pit_features$starter_ERA, na.rm = TRUE)))
  cat(sprintf("  Avg bullpen ERA:         %.2f\n",
              mean(pit_features$bullpen_ERA, na.rm = TRUE)))
  cat(sprintf("  Avg starter IP/game:     %.2f\n",
              mean(pit_features$starter_IP_per_game, na.rm = TRUE)))
  cat(sprintf("  Avg starter pct IP:      %.2f\n",
              mean(pit_features$starter_pct_IP, na.rm = TRUE)))

  # Join pitcher features into pre-deadline team features
  pre <- pre %>%
    left_join(pit_features, by = c("Team", "Season"))

  cat("Pitcher features joined into pre-deadline feature matrix.\n")
  cat(sprintf("  Missing starter_ERA:  %d\n",
              sum(is.na(pre$starter_ERA))))
  cat(sprintf("  Missing bullpen_ERA:  %d\n",
              sum(is.na(pre$bullpen_ERA))))

} else {
  cat("No pitcher batch files found — skipping pitcher split features.\n")
  cat("Place pitcher_batch_YYYY_YYYY.csv files in the working directory.\n")
}

# ── OPPONENT FEATURES ─────────────────────────────────────────────────────────
opp_features <- pre %>%
  rename_with(~ paste0("opp_", .), -c(Team, Season)) %>%
  rename(Opp = Team)

# ── BUILD POST-DEADLINE PREDICTION ROWS ───────────────────────────────────────
# Add second_half_game_num directly on the post-deadline rows before joining
# This avoids the many-to-many ambiguity caused by doubleheaders sharing
# the same Team + Season + Date key

post <- df %>%
  filter(is_post_deadline == 1) %>%
  arrange(Team, Season, Date) %>%
  group_by(Team, Season) %>%
  mutate(second_half_game_num = row_number()) %>%
  ungroup() %>%
  left_join(pre,          by = c("Team", "Season")) %>%
  left_join(opp_features, by = c("Opp",  "Season"))


# ── FINAL FEATURE MATRIX ──────────────────────────────────────────────────────
model_df <- post %>%
  select(
    # Identifiers
    Team, Season, Date, Opp,

    # Game context
    is_home,
    is_shortened_season,
    second_half_game_num,

    # Own pre-deadline features
    n_pre_games,
    starts_with("cum_"),
    starts_with("cv_"),
    starts_with("q25_"),
    starts_with("q75_"),
    starts_with("iqr_"),
    starts_with("roll_"),
    starts_with("slope_"),
    starts_with("starter_"),
    starts_with("bullpen_"),

    # Opponent pre-deadline features
    starts_with("opp_cum_"),
    starts_with("opp_cv_"),
    starts_with("opp_q25_"),
    starts_with("opp_q75_"),
    starts_with("opp_iqr_"),
    starts_with("opp_roll_"),
    starts_with("opp_slope_"),
    starts_with("opp_starter_"),
    starts_with("opp_bullpen_"),

    # Targets
    runs_scored  = bat_R,
    runs_allowed = opp_score,
    win
  )

# ── SAVE ──────────────────────────────────────────────────────────────────────
write.csv(model_df, "../data/model_features.csv", row.names = FALSE)
cat("\nSaved model_features.csv\n")
cat("Next step: run 02_train_models.R\n")
