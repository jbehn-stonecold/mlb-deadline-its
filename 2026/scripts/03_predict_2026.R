library(dplyr)
library(catboost)

# 0. LOAD DATA
df      <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded model_features.csv:", nrow(df), "rows\n")

pred_df <- df %>% filter(Season == 2026)
cat("2026 post-deadline rows:", nrow(pred_df), "\n")
cat("Teams:", length(unique(pred_df$Team)), "\n")

# 1. LOAD MODELS
model_rs <- catboost.load_model("../models/model_runs_scored.cbm")
model_ra <- catboost.load_model("../models/model_runs_allowed.cbm")
model_wl <- catboost.load_model("../models/model_win_loss.cbm")

# ── 2. FEATURE COLUMNS ────────────────────────────────────────────────────────
# RS and RA use SHAP-selected ordered feature lists from optimization run.
# Team and Opp are handled as categoricals — NOT appended separately.
# WL uses full feature set minus id and target columns.
id_cols     <- c("Team", "Season", "Date", "Opp")
target_cols <- c("runs_scored", "runs_allowed", "win")

feat_cols_rs <- readLines("../models/opt_features_runs_scored.txt")
feat_cols_ra <- readLines("../models/opt_features_runs_allowed.txt")
feat_cols_wl <- readLines("../models/opt_features_win_loss.txt")
feat_cols_wl <- feat_cols_wl[feat_cols_wl %in% names(df)]

cat("RS features:", length(feat_cols_rs),
    "| RA features:", length(feat_cols_ra),
    "| WL features:", length(feat_cols_wl), "\n")

# ── 3. HELPERS ────────────────────────────────────────────────────────────────
make_pool_with_cats <- function(data, target_col, feat_cols, cat_cols) {
  # feat_cols does NOT include cat_cols — append them for pool building
  all_cols <- c(feat_cols, cat_cols)
  X <- data[, all_cols]
  y <- data[[target_col]]
  for (col in cat_cols) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

make_pool_no_cats <- function(data, target_col, feat_cols) {
  # feat_cols already in correct order, no cats to append
  X <- data[, feat_cols]
  y <- data[[target_col]]
  X[["Team"]] <- as.factor(X[["Team"]])
  X[["Opp"]]  <- as.factor(X[["Opp"]])
  cat_idx <- which(names(X) %in% c("Team", "Opp")) - 1L
  catboost.load_pool(data = X, label = y,
                     cat_features = cat_idx)
}

logit_to_prob <- function(x) 1 / (1 + exp(-x))
pyth_win_prob <- function(rs, ra, exp = 1.83) rs^exp / (rs^exp + ra^exp)

# 4. GAME-LEVEL PREDICTIONS

# RS and RA: features from text files don't include Team/Opp so append them
pool_rs <- make_pool_with_cats(pred_df, "runs_scored",  feat_cols_rs, c("Team","Opp"))
pool_ra <- make_pool_with_cats(pred_df, "runs_allowed", feat_cols_ra, c("Team","Opp"))

# WL: full feature set which already includes Team and Opp via feat_cols_wl
pool_wl <- make_pool_with_cats(pred_df, "win", 
                                feat_cols_wl[!feat_cols_wl %in% c("Team","Opp")],
                                c("Team","Opp"))

pred_df$pred_runs_scored  <- catboost.predict(model_rs, pool_rs)
pred_df$pred_runs_allowed <- catboost.predict(model_ra, pool_ra)
pred_df$pred_win_prob     <- logit_to_prob(catboost.predict(model_wl, pool_wl))
pred_df$pred_win          <- as.integer(pred_df$pred_win_prob >= 0.5)
pred_df$pred_win_rd       <- as.integer(pred_df$pred_runs_scored > pred_df$pred_runs_allowed)

cat(sprintf("\nRD vs W/L agreement: %d / %d games (%.1f%%)\n",
            sum(pred_df$pred_win == pred_df$pred_win_rd, na.rm = TRUE),
            nrow(pred_df),
            100 * mean(pred_df$pred_win == pred_df$pred_win_rd, na.rm = TRUE)))

# 5. AGGREGATE TO TEAM TOTALS

# RMSE from held-out test set — used to compute intervals
# TODO: replace with this cycle's actual 2025 test-set RMSE from
# 02b_test_results.csv once 02b_direct_retrain has been re-run on the
# shifted split. Values below are carried over from the prior (2024 test)
# cycle as placeholders and should NOT be trusted for real intervals.
RMSE_RS <- 3.0524
RMSE_RA <- 3.0535

# Z-scores for 90%, 95%, 99% confidence intervals
Z90 <- 1.645
Z95 <- 1.960
Z99 <- 2.576

# Significance stars helper — based on whether CI excludes zero
# Uses 90/95/99% CI on the IMPACT (actual minus projected)
# *** p<=0.01  ** p<=0.05  * p<=0.10  (blank) not significant
impact_stars <- function(impact, sd_proj, n_games = NULL, type = "wins") {
  # For wins: sd_proj is the Bernoulli SD of projected wins
  # For RS/RA per game: sd_proj is RMSE/sqrt(n)
  # CI on impact = impact ± Z * sd_proj (actual is fixed, uncertainty in projection)
  stars_fn <- function(z_val) {
    lo <- impact - z_val * sd_proj
    hi <- impact + z_val * sd_proj
    !(lo <= 0 & hi >= 0)  # TRUE = CI excludes zero = significant
  }
  dplyr::case_when(
    stars_fn(Z99) ~ "***",
    stars_fn(Z95) ~ "**",
    stars_fn(Z90) ~ "*",
    TRUE          ~ ""
  )
}

team_summary <- pred_df %>%
  group_by(Team) %>%
  summarise(
    games_played       = n(),
    actual_wins        = sum(win,           na.rm = TRUE),
    actual_losses      = games_played - actual_wins,
    actual_win_pct     = round(actual_wins / games_played, 3),
    actual_rs_total    = sum(runs_scored,   na.rm = TRUE),
    actual_ra_total    = sum(runs_allowed,  na.rm = TRUE),
    actual_rs_per_game = round(actual_rs_total / games_played, 2),
    actual_ra_per_game = round(actual_ra_total / games_played, 2),
    actual_run_diff    = actual_rs_total - actual_ra_total,
    proj_wins          = round(sum(pred_win_prob,     na.rm = TRUE)),
    proj_losses        = games_played - proj_wins,
    proj_win_pct       = round(proj_wins / games_played, 3),
    proj_rs_total      = round(sum(pred_runs_scored,  na.rm = TRUE), 1),
    proj_ra_total      = round(sum(pred_runs_allowed, na.rm = TRUE), 1),
    proj_rs_per_game   = round(proj_rs_total / games_played, 2),
    proj_ra_per_game   = round(proj_ra_total / games_played, 2),
    proj_run_diff      = round(proj_rs_total - proj_ra_total, 1),
    proj_wins_rd       = sum(pred_win_rd,   na.rm = TRUE),
    proj_win_pct_rd    = round(proj_wins_rd / games_played, 3),
    impact_wins        = actual_wins - proj_wins,
    impact_wins_rd     = actual_wins - proj_wins_rd,
    impact_win_pct     = round(actual_win_pct - proj_win_pct, 3),
    impact_win_pct_rd  = round(actual_win_pct - proj_win_pct_rd, 3),
    impact_rs_total    = round(actual_rs_total - proj_rs_total, 1),
    impact_ra_total    = round(actual_ra_total - proj_ra_total, 1),
    impact_rs_per_game = round(actual_rs_per_game - proj_rs_per_game, 2),
    impact_ra_per_game = round(actual_ra_per_game - proj_ra_per_game, 2),
    impact_run_diff    = round(actual_run_diff - proj_run_diff, 1),

    # CONFIDENCE INTERVALS — 90% PRIMARY (shown on chart)
    # Wins: Bernoulli variance SD = sqrt(sum(p*(1-p)))
    proj_wins_sd       = round(sqrt(sum(pred_win_prob * (1 - pred_win_prob),
                                        na.rm = TRUE)), 2),
    proj_wins_ci90_lo  = round(proj_wins - Z90 * proj_wins_sd),
    proj_wins_ci90_hi  = round(proj_wins + Z90 * proj_wins_sd),
    proj_wins_ci95_lo  = round(proj_wins - Z95 * proj_wins_sd),
    proj_wins_ci95_hi  = round(proj_wins + Z95 * proj_wins_sd),
    proj_wins_ci99_lo  = round(proj_wins - Z99 * proj_wins_sd),
    proj_wins_ci99_hi  = round(proj_wins + Z99 * proj_wins_sd),
    # Keep ci_lo/hi at 90% for chart labels
    proj_wins_ci_lo    = proj_wins_ci90_lo,
    proj_wins_ci_hi    = proj_wins_ci90_hi,

    # RS/G CI: RMSE / sqrt(n)
    rs_ci_margin_90    = round(Z90 * RMSE_RS / sqrt(games_played), 2),
    rs_ci_margin_95    = round(Z95 * RMSE_RS / sqrt(games_played), 2),
    rs_ci_margin_99    = round(Z99 * RMSE_RS / sqrt(games_played), 2),
    proj_rs_pg_ci_lo   = round(proj_rs_per_game - rs_ci_margin_90, 2),
    proj_rs_pg_ci_hi   = round(proj_rs_per_game + rs_ci_margin_90, 2),

    # RA/G CI
    ra_ci_margin_90    = round(Z90 * RMSE_RA / sqrt(games_played), 2),
    ra_ci_margin_95    = round(Z95 * RMSE_RA / sqrt(games_played), 2),
    ra_ci_margin_99    = round(Z99 * RMSE_RA / sqrt(games_played), 2),
    proj_ra_pg_ci_lo   = round(proj_ra_per_game - ra_ci_margin_90, 2),
    proj_ra_pg_ci_hi   = round(proj_ra_per_game + ra_ci_margin_90, 2),

    # Single-game prediction interval (95%)
    rs_pi_margin       = round(Z95 * RMSE_RS, 2),
    ra_pi_margin       = round(Z95 * RMSE_RA, 2),

    .groups = "drop"
  ) %>%
  # ── FORECAST COVERAGE STARS ───────────────────────────────────────────────
  # Stars = forecast coverage accuracy (how close actual was to projection).
  # *** = actual within 90% CI — tightest band, best prediction
  # **  = actual within 95% CI but outside 90% CI
  # *   = actual within 99% CI but outside 95% CI
  # (blank) = actual outside 99% CI — model significantly missed this team
  mutate(
    wins_se = proj_wins_sd,
    rs_se   = rs_ci_margin_90 / Z90,
    ra_se   = ra_ci_margin_90 / Z90,
    wins_stars = case_when(
      abs(impact_wins) <= Z90 * wins_se ~ "***",
      abs(impact_wins) <= Z95 * wins_se ~ "**",
      abs(impact_wins) <= Z99 * wins_se ~ "*",
      TRUE ~ ""
    ),
    rs_stars = case_when(
      abs(impact_rs_per_game) <= Z90 * rs_se ~ "***",
      abs(impact_rs_per_game) <= Z95 * rs_se ~ "**",
      abs(impact_rs_per_game) <= Z99 * rs_se ~ "*",
      TRUE ~ ""
    ),
    ra_stars = case_when(
      abs(impact_ra_per_game) <= Z90 * ra_se ~ "***",
      abs(impact_ra_per_game) <= Z95 * ra_se ~ "**",
      abs(impact_ra_per_game) <= Z99 * ra_se ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(desc(impact_wins))

# 6. PYTHAGOREAN SANITY CHECK
team_summary <- team_summary %>%
  mutate(
    pyth_proj_win_pct = round(pyth_win_prob(proj_rs_per_game, proj_ra_per_game), 3),
    pyth_vs_model_gap = round(proj_win_pct - pyth_proj_win_pct, 3)
  )

# 7. PRINT RESULTS

cat("── Win Impact (W/L model) ──\n")
team_summary %>%
  select(Team, games_played, proj_wins, actual_wins, impact_wins,
         proj_win_pct, actual_win_pct, impact_win_pct) %>%
  arrange(desc(impact_wins)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Runs Scored Impact ──\n")
team_summary %>%
  select(Team, proj_rs_per_game, actual_rs_per_game, impact_rs_per_game,
         proj_rs_total, actual_rs_total, impact_rs_total) %>%
  arrange(desc(impact_rs_total)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Runs Allowed Impact ──\n")
team_summary %>%
  select(Team, proj_ra_per_game, actual_ra_per_game, impact_ra_per_game,
         proj_ra_total, actual_ra_total, impact_ra_total) %>%
  arrange(impact_ra_total) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Run Differential Impact ──\n")
team_summary %>%
  select(Team, proj_run_diff, actual_run_diff, impact_run_diff) %>%
  arrange(desc(impact_run_diff)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Pythagorean Sanity Check ──\n")
team_summary %>%
  select(Team, proj_win_pct, pyth_proj_win_pct, pyth_vs_model_gap) %>%
  arrange(desc(abs(pyth_vs_model_gap))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Confidence & Prediction Intervals ──\n")
cat("95% CI on projected wins: proj_wins ± 1.96 * sqrt(sum(p*(1-p)))\n")
cat("95% CI on projected RS/G and RA/G: proj_mean ± 1.96 * RMSE / sqrt(n)\n")
cat("95% PI on single game RS/RA: pred ± 1.96 * RMSE = ±",
    round(Z95 * RMSE_RS, 2), "runs\n\n")

team_summary %>%
  select(Team, proj_wins, proj_wins_ci_lo, proj_wins_ci_hi,
         proj_rs_per_game, proj_rs_pg_ci_lo, proj_rs_pg_ci_hi,
         proj_ra_per_game, proj_ra_pg_ci_lo, proj_ra_pg_ci_hi,
         actual_wins, impact_wins) %>%
  arrange(desc(impact_wins)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("W/L model = classification model | RD method = proj RS > proj RA per game\n\n")

method_compare <- team_summary %>%
  select(Team, actual_wins, proj_wins, proj_wins_rd) %>%
  mutate(wl_error = actual_wins - proj_wins,
         rd_error = actual_wins - proj_wins_rd) %>%
  arrange(desc(actual_wins))

print(as.data.frame(method_compare), row.names = FALSE)

cat(sprintf("\n  W/L model  MAE: %.2f wins | RMSE: %.2f wins\n",
            mean(abs(method_compare$wl_error)),
            sqrt(mean(method_compare$wl_error^2))))
cat(sprintf("  RD method  MAE: %.2f wins | RMSE: %.2f wins\n",
            mean(abs(method_compare$rd_error)),
            sqrt(mean(method_compare$rd_error^2))))

# 8. SAVE
write.csv(pred_df,      "../outputs/predictions_2026_game_level.csv",  row.names = FALSE)
write.csv(team_summary, "../outputs/predictions_2026_team_summary.csv", row.names = FALSE)
