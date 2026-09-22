library(dplyr)
library(catboost)

# ════════════════════════════════════════════════════════════════════════════════
#  SCRIPT 3 OF 3 — 2025 PREDICTIONS & TRADE DEADLINE IMPACT ESTIMATION
#  Input:  model_features.csv
#          model_runs_scored.cbm
#          model_runs_allowed.cbm
#          model_win_loss.cbm
#  Output: predictions_2025_game_level.csv
#          predictions_2025_team_summary.csv
#
#  For each post-deadline game in 2025, the three models project what the
#  team would have done based purely on their pre-deadline first-half profile
#  and historical patterns — as if no deadline moves were made.
#
#  The gap between these projections and actual results is the estimated
#  cumulative impact of trade deadline activity for each team.
#
#  Interpretation note:
#    This is a forecasting model, not a pure causal model. Every historical
#    second half used for training was itself affected by that year's deadline.
#    The delta therefore measures how much each team over- or under-performed
#    their historical peer expectation, not a clean causal estimate of the
#    deadline's effect. Results should be interpreted accordingly.
# ════════════════════════════════════════════════════════════════════════════════

# ── 0. LOAD ────────────────────────────────────────────────────────────────────
df      <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded model_features.csv:", nrow(df), "rows\n")

# ── 1. ISOLATE 2025 ───────────────────────────────────────────────────────────
pred_df <- df %>% filter(Season == 2025)
cat("2025 post-deadline rows:", nrow(pred_df), "\n")
cat("Teams:", length(unique(pred_df$Team)), "\n")

# ── 2. FEATURE COLUMNS ────────────────────────────────────────────────────────
id_cols      <- c("Team", "Season", "Date", "Opp")
target_cols  <- c("runs_scored", "runs_allowed", "win")
cat_features <- c("Team", "Opp")
all_cols     <- names(df)

# RS and RA models used SHAP-selected feature subsets — load the saved lists
# which preserve the exact feature order the models were trained with.
# WL model used the full feature set so we derive it normally.
feat_cols_rs <- readLines("opt_features_runs_scored.txt")
feat_cols_ra <- readLines("opt_features_runs_allowed.txt")
feat_cols_wl <- setdiff(all_cols, c(id_cols, target_cols))

cat("RS features:", length(feat_cols_rs),
    "| RA features:", length(feat_cols_ra),
    "| WL features:", length(feat_cols_wl), "\n")

# ── 3. LOAD MODELS ────────────────────────────────────────────────────────────
cat("\nLoading saved models...\n")
model_rs <- catboost.load_model("../models/model_runs_scored.cbm")
model_ra <- catboost.load_model("../models/model_runs_allowed.cbm")
model_wl <- catboost.load_model("../models/model_win_loss.cbm")
cat("Models loaded.\n")

# ── 4. HELPERS ────────────────────────────────────────────────────────────────
make_pool <- function(data, target_col, feature_cols, cat_features) {
  all_feat_cols <- c(feature_cols, cat_features)
  X <- data[, all_feat_cols]
  y <- data[[target_col]]
  for (col in cat_features) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

logit_to_prob <- function(x) 1 / (1 + exp(-x))

pyth_win_prob <- function(rs, ra, exp = 1.83) rs^exp / (rs^exp + ra^exp)

# ── 5. GAME-LEVEL PREDICTIONS ─────────────────────────────────────────────────
cat("\nGenerating 2025 game-level predictions...\n")

pool_rs <- make_pool(pred_df, "runs_scored",  feat_cols_rs, cat_features)
pool_ra <- make_pool(pred_df, "runs_allowed", feat_cols_ra, cat_features)
pool_wl <- make_pool(pred_df, "win",          feat_cols_wl, cat_features)

pred_df$pred_runs_scored  <- catboost.predict(model_rs, pool_rs)
pred_df$pred_runs_allowed <- catboost.predict(model_ra, pool_ra)
pred_df$pred_win_prob     <- logit_to_prob(catboost.predict(model_wl, pool_wl))
pred_df$pred_win          <- as.integer(pred_df$pred_win_prob >= 0.5)

cat("Game-level predictions complete.\n")

# ── 6. AGGREGATE TO SECOND-HALF TEAM TOTALS ───────────────────────────────────
cat("\nAggregating to second-half totals per team...\n")

team_summary <- pred_df %>%
  group_by(Team) %>%
  summarise(
    games_played = n(),

    # Actual second-half results
    actual_wins        = sum(win,          na.rm = TRUE),
    actual_losses      = games_played - actual_wins,
    actual_win_pct     = round(actual_wins / games_played, 3),
    actual_rs_total    = sum(runs_scored,  na.rm = TRUE),
    actual_ra_total    = sum(runs_allowed, na.rm = TRUE),
    actual_rs_per_game = round(actual_rs_total / games_played, 2),
    actual_ra_per_game = round(actual_ra_total / games_played, 2),
    actual_run_diff    = actual_rs_total - actual_ra_total,

    # Projected second-half (counterfactual — historical peer expectation)
    # proj_wins uses sum of probabilities rather than binary threshold
    # to avoid overconfident win/loss projections
    proj_wins          = round(sum(pred_win_prob,     na.rm = TRUE)),
    proj_losses        = games_played - proj_wins,
    proj_win_pct       = round(proj_wins / games_played, 3),
    proj_rs_total      = round(sum(pred_runs_scored,  na.rm = TRUE), 1),
    proj_ra_total      = round(sum(pred_runs_allowed, na.rm = TRUE), 1),
    proj_rs_per_game   = round(proj_rs_total / games_played, 2),
    proj_ra_per_game   = round(proj_ra_total / games_played, 2),
    proj_run_diff      = round(proj_rs_total - proj_ra_total, 1),

    # Impact estimates (actual minus projected)
    # Positive = outperformed historical peer expectation
    # Negative = underperformed historical peer expectation
    impact_wins        = actual_wins - proj_wins,
    impact_win_pct     = round(actual_win_pct - proj_win_pct, 3),
    impact_rs_total    = round(actual_rs_total - proj_rs_total, 1),
    impact_ra_total    = round(actual_ra_total - proj_ra_total, 1),
    impact_rs_per_game = round(actual_rs_per_game - proj_rs_per_game, 2),
    impact_ra_per_game = round(actual_ra_per_game - proj_ra_per_game, 2),
    impact_run_diff    = round(actual_run_diff - proj_run_diff, 1),

    .groups = "drop"
  ) %>%
  arrange(desc(impact_wins))

# ── 7. PYTHAGOREAN SANITY CHECK ───────────────────────────────────────────────
team_summary <- team_summary %>%
  mutate(
    pyth_proj_win_pct = round(pyth_win_prob(proj_rs_per_game, proj_ra_per_game), 3),
    pyth_vs_model_gap = round(proj_win_pct - pyth_proj_win_pct, 3)
  )

# ── 8. PRINT RESULTS ──────────────────────────────────────────────────────────
cat("\n════════════════════════════════════════════════════════\n")
cat("  2025 TRADE DEADLINE IMPACT — SECOND HALF RESULTS\n")
cat("════════════════════════════════════════════════════════\n\n")

cat("── Win Impact (Actual minus Projected) ──\n")
team_summary %>%
  select(Team, games_played, proj_wins, actual_wins, impact_wins,
         proj_win_pct, actual_win_pct, impact_win_pct) %>%
  arrange(desc(impact_wins)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n── Runs Scored Impact ──\n")
team_summary %>%
  select(Team, proj_rs_per_game, actual_rs_per_game, impact_rs_per_game,
         proj_rs_total, actual_rs_total, impact_rs_total) %>%
  arrange(desc(impact_rs_total)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n── Runs Allowed Impact ──\n")
team_summary %>%
  select(Team, proj_ra_per_game, actual_ra_per_game, impact_ra_per_game,
         proj_ra_total, actual_ra_total, impact_ra_total) %>%
  arrange(impact_ra_total) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n── Run Differential Impact ──\n")
team_summary %>%
  select(Team, proj_run_diff, actual_run_diff, impact_run_diff) %>%
  arrange(desc(impact_run_diff)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("\n── Pythagorean Sanity Check ──\n")
cat("Gaps > 0.030 may indicate model inconsistency worth investigating.\n")
team_summary %>%
  select(Team, proj_win_pct, pyth_proj_win_pct, pyth_vs_model_gap) %>%
  arrange(desc(abs(pyth_vs_model_gap))) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# ── 9. SAVE ───────────────────────────────────────────────────────────────────
write.csv(pred_df,      "../outputs/predictions_2025_game_level.csv",  row.names = FALSE)
write.csv(team_summary, "../outputs/predictions_2025_team_summary.csv", row.names = FALSE)

cat("\nSaved:\n")
cat("  predictions_2025_game_level.csv\n")
cat("  predictions_2025_team_summary.csv\n")
cat("\nDone.\n")
