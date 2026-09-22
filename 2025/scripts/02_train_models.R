library(dplyr)
library(catboost)

# ════════════════════════════════════════════════════════════════════════════════
#  SCRIPT 2 OF 3 — MODEL TRAINING
#  Input:  model_features.csv
#  Output: model_runs_scored.cbm
#          model_runs_allowed.cbm
#          model_win_loss.cbm
#          test_predictions.csv
#
#  Three CatBoost models are trained:
#    1. Runs Scored  — regression, primary input: batting features
#    2. Runs Allowed — regression, primary input: pitching features
#    3. Win / Loss   — classification, input: all features
#
#  Validation strategy:
#    Train:      2005-2021
#    Validation: 2022-2023  (hyperparameter tuning / early stopping)
#    Test:       2024       (touched once at the end — true holdout)
#    Predict:    2025       (counterfactual projections)
#
#  Rolling window CV is run before final training to confirm stability.
#  Pythagorean expectation baseline is computed for comparison.
# ════════════════════════════════════════════════════════════════════════════════

# ── 0. LOAD ────────────────────────────────────────────────────────────────────
df <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# ── 1. DEFINE FEATURE COLUMNS ─────────────────────────────────────────────────
id_cols     <- c("Team", "Season", "Date", "Opp")
target_cols <- c("runs_scored", "runs_allowed", "win")
cat_features <- c("Team", "Opp")

all_cols <- names(df)

# ── Runs Scored features ──────────────────────────────────────────────────────
# Own batting + opponent pitching + game context
# Excludes: own pitching stats, opponent batting stats
rs_exclude <- c(
  id_cols, target_cols,
  # own pitching — not relevant to how many runs your offense scores
  grep("^(cum_ER|cum_H_allowed|cum_HR_allowed|cum_BB_allowed|cum_SO_pit|cum_HBP_pit|cum_WP|cum_ERA|cum_RA|roll_ERA|roll_SO_pit|roll_BB_allowed|roll_RA|slope_RA|slope_ERA|starter_|bullpen_)",
       all_cols, value = TRUE),
  # opponent batting — opponent offense does not determine your runs scored
  grep("^opp_(cum_R|cum_H_per|cum_HR_per|cum_BB_per|cum_SO_per|cum_RBI|cum_SB|cum_LOB|cum_OBP|cum_SLG|cum_OPS|cum_BA|cum_XBH|cum_WPA|cum_RE24|cum_run_diff|roll_R|roll_OBP|roll_SLG|roll_OPS|roll_HR|roll_BB_per|roll_SO_per|roll_win|roll_run_diff|slope_R|slope_win|slope_OPS|slope_run|q25_R|q75_R|iqr_R|q25_run|q75_run|iqr_run|cv_run)",
       all_cols, value = TRUE)
)
feat_cols_rs <- setdiff(all_cols, rs_exclude)
cat("Runs Scored features:", length(feat_cols_rs), "\n")

# ── Runs Allowed features ─────────────────────────────────────────────────────
# Own pitching + opponent batting + game context
# Excludes: own batting stats, opponent pitching stats
ra_exclude <- c(
  id_cols, target_cols,
  # own batting — not relevant to how many runs your pitching allows
  grep("^(cum_R_per|cum_H_per|cum_HR_per|cum_BB_per|cum_SO_per|cum_RBI|cum_SB|cum_LOB|cum_OBP|cum_SLG|cum_OPS|cum_BA|cum_XBH|cum_WPA|cum_RE24|roll_R_per|roll_OBP|roll_SLG|roll_OPS|roll_HR|roll_BB_per|roll_SO_per|slope_R|slope_OPS|q25_R_per|q75_R_per|iqr_R_per)",
       all_cols, value = TRUE),
  # opponent pitching — opponent staff does not determine your runs allowed
  grep("^opp_(cum_ER|cum_H_allowed|cum_HR_allowed|cum_BB_allowed|cum_SO_pit|cum_HBP_pit|cum_WP|cum_ERA|cum_RA|roll_ERA|roll_SO_pit|roll_BB_allowed|roll_RA|slope_RA|slope_ERA|starter_|bullpen_)",
       all_cols, value = TRUE)
)
feat_cols_ra <- setdiff(all_cols, ra_exclude)
cat("Runs Allowed features:", length(feat_cols_ra), "\n")

# ── Win/Loss features — all features ─────────────────────────────────────────
feat_cols_wl <- setdiff(all_cols, c(id_cols, target_cols))
cat("Win/Loss features:", length(feat_cols_wl), "\n")

# ── 2. DATA SPLITS ────────────────────────────────────────────────────────────
train_df <- df %>% filter(Season <= 2021)
val_df   <- df %>% filter(Season %in% c(2022, 2023))
test_df  <- df %>% filter(Season == 2024)
pred_df  <- df %>% filter(Season == 2025)

cat("\nSplit sizes:\n")
cat("  Train:", nrow(train_df), "rows (2005-2021)\n")
cat("  Val:  ", nrow(val_df),   "rows (2022-2023)\n")
cat("  Test: ", nrow(test_df),  "rows (2024)\n")
cat("  2025: ", nrow(pred_df),  "rows\n")

# ── 3. HELPER FUNCTIONS ────────────────────────────────────────────────────────
make_pool <- function(data, target_col, feature_cols, cat_features) {
  all_feat_cols <- c(feature_cols, cat_features)
  X <- data[, all_feat_cols]
  y <- data[[target_col]]
  for (col in cat_features) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

accuracy <- function(actual, predicted_prob, threshold = 0.5) {
  mean(as.integer(predicted_prob >= threshold) == actual, na.rm = TRUE)
}

logit_to_prob <- function(x) 1 / (1 + exp(-x))

# ── 4. PYTHAGOREAN BASELINE ────────────────────────────────────────────────────
pyth_win_prob <- function(rs, ra, exp = 1.83) rs^exp / (rs^exp + ra^exp)

cat("\n── Pythagorean Baseline (Test 2024) ──\n")
pyth_prob    <- pyth_win_prob(test_df$cum_R_per_game, test_df$cum_RA_per_game)
pyth_acc     <- accuracy(test_df$win, pyth_prob)
pyth_rmse_rs <- rmse(test_df$runs_scored,  test_df$cum_R_per_game)
pyth_rmse_ra <- rmse(test_df$runs_allowed, test_df$cum_RA_per_game)
cat("  Win accuracy:      ", round(pyth_acc,     4), "\n")
cat("  Runs scored RMSE:  ", round(pyth_rmse_rs, 4), "\n")
cat("  Runs allowed RMSE: ", round(pyth_rmse_ra, 4), "\n")

# ── 5. CATBOOST PARAMETERS ────────────────────────────────────────────────────
# Parameters confirmed via rolling window grid search:
#   Regression:     depth=6, lr=0.05, l2=3
#   Classification: depth=4, lr=0.02, l2=10

cb_params_reg <- list(
  loss_function  = "RMSE",
  iterations     = 500,
  learning_rate  = 0.05,
  depth          = 6,
  l2_leaf_reg    = 3,
  od_type        = "Iter",
  od_wait        = 50,
  use_best_model = TRUE,
  random_seed    = 42,
  verbose        = 100
)

cb_params_cls <- list(
  loss_function  = "Logloss",
  iterations     = 500,
  learning_rate  = 0.05,
  depth          = 6,
  l2_leaf_reg    = 3,
  od_type        = "Iter",
  od_wait        = 50,
  use_best_model = TRUE,
  random_seed    = 42,
  verbose        = 100
)

# ── 6. ROLLING WINDOW CROSS-VALIDATION ────────────────────────────────────────
rolling_folds <- list(
  list(train_end = 2018, val_year = 2019),
  list(train_end = 2019, val_year = 2020),
  list(train_end = 2020, val_year = 2021),
  list(train_end = 2021, val_year = 2022)
)

run_rolling_cv <- function(target_col, params, metric_fn, metric_name,
                           feat_cols, is_classification = FALSE) {
  cat(sprintf("\n── Rolling CV: %s ──\n", target_col))
  scores <- numeric(length(rolling_folds))

  for (i in seq_along(rolling_folds)) {
    fold    <- rolling_folds[[i]]
    tr      <- df %>% filter(Season <= fold$train_end)
    va      <- df %>% filter(Season == fold$val_year)
    tr_pool <- make_pool(tr, target_col, feat_cols, cat_features)
    va_pool <- make_pool(va, target_col, feat_cols, cat_features)

    model   <- catboost.train(
      learn_pool = tr_pool,
      test_pool  = va_pool,
      params     = params
    )

    preds <- catboost.predict(model, va_pool)
    if (is_classification) preds <- logit_to_prob(preds)

    score     <- metric_fn(va[[target_col]], preds)
    scores[i] <- score
    cat(sprintf("  Fold %d | train <= %d, val = %d | %s = %.4f\n",
                i, fold$train_end, fold$val_year, metric_name, score))
  }

  cat(sprintf("  Mean CV %s: %.4f\n", metric_name, mean(scores)))
  invisible(scores)
}

# ── 7. FINAL MODEL TRAINING ───────────────────────────────────────────────────
train_final_model <- function(target_col, params, feat_cols, label) {
  cat(sprintf("\n── Final Model: %s ──\n", label))
  tr_pool <- make_pool(train_df, target_col, feat_cols, cat_features)
  va_pool <- make_pool(val_df,   target_col, feat_cols, cat_features)
  model   <- catboost.train(
    learn_pool = tr_pool,
    test_pool  = va_pool,
    params     = params
  )
  cat(sprintf("  Trees used: %d\n", model$tree_count))
  return(model)
}

evaluate_model <- function(model, data, target_col, metric_fn, metric_name,
                           feat_cols, is_classification = FALSE) {
  pool  <- make_pool(data, target_col, feat_cols, cat_features)
  preds <- catboost.predict(model, pool)
  if (is_classification) preds <- logit_to_prob(preds)
  score <- metric_fn(data[[target_col]], preds)
  cat(sprintf("  %s: %.4f\n", metric_name, score))
  invisible(preds)
}

# ── 8. RUNS SCORED ────────────────────────────────────────────────────────────
run_rolling_cv("runs_scored", cb_params_reg, rmse, "RMSE", feat_cols_rs)
model_rs <- train_final_model("runs_scored", cb_params_reg, feat_cols_rs, "Runs Scored")

cat("\nRuns Scored — Val (2022-2023):\n")
pred_rs_val  <- evaluate_model(model_rs, val_df,  "runs_scored", rmse, "RMSE", feat_cols_rs)
cat("Runs Scored — Test (2024):\n")
pred_rs_test <- evaluate_model(model_rs, test_df, "runs_scored", rmse, "RMSE", feat_cols_rs)
cat(sprintf("  Pythagorean baseline RMSE: %.4f\n", pyth_rmse_rs))

# ── 9. RUNS ALLOWED ───────────────────────────────────────────────────────────
run_rolling_cv("runs_allowed", cb_params_reg, rmse, "RMSE", feat_cols_ra)
model_ra <- train_final_model("runs_allowed", cb_params_reg, feat_cols_ra, "Runs Allowed")

cat("\nRuns Allowed — Val (2022-2023):\n")
pred_ra_val  <- evaluate_model(model_ra, val_df,  "runs_allowed", rmse, "RMSE", feat_cols_ra)
cat("Runs Allowed — Test (2024):\n")
pred_ra_test <- evaluate_model(model_ra, test_df, "runs_allowed", rmse, "RMSE", feat_cols_ra)
cat(sprintf("  Pythagorean baseline RMSE: %.4f\n", pyth_rmse_ra))

# ── 10. WIN / LOSS ────────────────────────────────────────────────────────────
run_rolling_cv("win", cb_params_cls,
               function(a, p) accuracy(a, p), "Accuracy",
               feat_cols_wl, is_classification = TRUE)
model_wl <- train_final_model("win", cb_params_cls, feat_cols_wl, "Win/Loss")

cat("\nWin/Loss — Val (2022-2023):\n")
pred_wl_val  <- evaluate_model(model_wl, val_df,  "win",
                               function(a, p) accuracy(a, p), "Accuracy",
                               feat_cols_wl, is_classification = TRUE)
cat("Win/Loss — Test (2024):\n")
pred_wl_test <- evaluate_model(model_wl, test_df, "win",
                               function(a, p) accuracy(a, p), "Accuracy",
                               feat_cols_wl, is_classification = TRUE)
cat(sprintf("  Pythagorean baseline accuracy: %.4f\n", pyth_acc))

# ── 11. FEATURE IMPORTANCE ────────────────────────────────────────────────────
cat("\n── Top 15 Features ──\n")

print_importance <- function(model, feat_cols, label, n = 15) {
  all_feat_cols <- c(feat_cols, cat_features)
  imp    <- catboost.get_feature_importance(model, type = "FeatureImportance")
  imp_df <- data.frame(feature = all_feat_cols, importance = imp) %>%
    arrange(desc(importance)) %>%
    head(n)
  cat(sprintf("\n%s:\n", label))
  print(imp_df, row.names = FALSE)
}

print_importance(model_rs, feat_cols_rs, "Runs Scored")
print_importance(model_ra, feat_cols_ra, "Runs Allowed")
print_importance(model_wl, feat_cols_wl, "Win/Loss")

# ── 12. SAVE ──────────────────────────────────────────────────────────────────
catboost.save_model(model_rs, "../models/model_runs_scored.cbm")
catboost.save_model(model_ra, "../models/model_runs_allowed.cbm")
catboost.save_model(model_wl, "../models/model_win_loss.cbm")

test_preds <- test_df %>%
  select(Team, Season, Date, Opp, runs_scored, runs_allowed, win) %>%
  mutate(
    pred_runs_scored  = pred_rs_test,
    pred_runs_allowed = pred_ra_test,
    pred_win_prob     = pred_wl_test
  )
write.csv(test_preds, "../outputs/test_predictions.csv", row.names = FALSE)

cat("\nSaved:\n")
cat("  model_runs_scored.cbm\n")
cat("  model_runs_allowed.cbm\n")
cat("  model_win_loss.cbm\n")
cat("  test_predictions.csv\n")
cat("\nNext step: run 03_predict_2025.R\n")
