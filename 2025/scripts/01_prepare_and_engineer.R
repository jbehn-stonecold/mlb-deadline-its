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
#    3. Splits each season at July 31 (universal trade deadline cutoff)
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
cat("Raw rows:", nrow(df_raw), "\n")

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
    pit_H        = H.1,
    pit_R        = R.1,
    pit_ER       = ER,
    pit_UER      = UER,
    pit_HR       = HR.1,
    pit_BB       = BB.1,
    pit_IBB      = IBB.1,
    pit_SO       = SO.1,
    pit_HBP      = HBP.1,
    pit_BK       = BK,
    pit_WP       = WP,
    pit_BF       = BF,
    pit_BR       = BR,
    pit_WPA      = WPA,
    Season       = scrape_year
  )

cat("Columns after selection:", ncol(df), "\n")

# ── 2. PARSE DATE ──────────────────────────────────────────────────────────────
df$Date <- as.Date(df$Date)

# ── 3. PARSE RESULT ───────────────────────────────────────────────────────────
df$WL         <- substr(df$Result, 1, 1)
scores        <- regmatches(df$Result, regexpr("[0-9]+-[0-9]+", df$Result))
score_split   <- strsplit(scores, "-")
df$team_score <- as.integer(sapply(score_split, `[`, 1))
df$opp_score  <- as.integer(sapply(score_split, `[`, 2))
df$win        <- as.integer(df$WL == "W")
df$Result     <- NULL
df$WL         <- NULL

# ── 4. RECODE HOME/AWAY ───────────────────────────────────────────────────────
df$is_home    <- as.integer(is.na(df$home_away))
df$home_away  <- NULL

# ── 5. UNIFY FRANCHISE IDENTIFIERS ───────────────────────────────────────────
df$Team <- dplyr::recode(df$Team, "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
df$Opp  <- dplyr::recode(df$Opp,  "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK")
cat("Unique teams after unification:", length(unique(df$Team)), "\n")

# ── 6. FLAGS ──────────────────────────────────────────────────────────────────
df$is_shortened_season <- as.integer(df$Season == 2020)

# ── 7. DEADLINE SPLIT ─────────────────────────────────────────────────────────
df$deadline_date    <- as.Date(paste0(df$Season, "-07-31"))
df$is_post_deadline <- as.integer(df$Date > df$deadline_date)
cat("Pre-deadline rows: ", sum(df$is_post_deadline == 0), "\n")
cat("Post-deadline rows:", sum(df$is_post_deadline == 1), "\n")

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

# ══════════════════════════════════════════════════════════════════════════════
#  FEATURE ENGINEERING
# ══════════════════════════════════════════════════════════════════════════════

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
cat("\nComputing pre-deadline features...\n")

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

cat("Pre-deadline feature rows:", nrow(pre), "\n")

# ── OPPONENT FEATURES ─────────────────────────────────────────────────────────
opp_features <- pre %>%
  rename_with(~ paste0("opp_", .), -c(Team, Season)) %>%
  rename(Opp = Team)

# ── BUILD POST-DEADLINE PREDICTION ROWS ───────────────────────────────────────
# Add second_half_game_num directly on the post-deadline rows before joining
# This avoids the many-to-many ambiguity caused by doubleheaders sharing
# the same Team + Season + Date key

cat("Building post-deadline prediction rows...\n")

post <- df %>%
  filter(is_post_deadline == 1) %>%
  arrange(Team, Season, Date) %>%
  group_by(Team, Season) %>%
  mutate(second_half_game_num = row_number()) %>%
  ungroup() %>%
  left_join(pre,          by = c("Team", "Season")) %>%
  left_join(opp_features, by = c("Opp",  "Season"))

cat("Post-deadline rows:", nrow(post), "\n")

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
    starts_with("roll_"),
    starts_with("slope_"),

    # Opponent pre-deadline features
    starts_with("opp_cum_"),
    starts_with("opp_cv_"),
    starts_with("opp_roll_"),
    starts_with("opp_slope_"),

    # Targets
    runs_scored  = bat_R,
    runs_allowed = opp_score,
    win
  )

cat("\nFinal model dataframe:\n")
cat("  Rows:    ", nrow(model_df), "\n")
cat("  Columns: ", ncol(model_df), "\n")
cat("  Seasons: ", paste(sort(unique(model_df$Season)), collapse = ", "), "\n")

cat("\nMissing values in feature columns:\n")
feature_cols <- grep("^(cum_|cv_|roll_|slope_|opp_|second_)",
                     names(model_df), value = TRUE)
missing <- colSums(is.na(model_df[, feature_cols]))
print(missing[missing > 0])

# ── SAVE ──────────────────────────────────────────────────────────────────────
write.csv(model_df, "../data/model_features.csv", row.names = FALSE)
cat("\nSaved model_features.csv\n")
cat("Next step: run 02_train_models.R\n")
