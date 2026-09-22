library(dplyr)
library(catboost)

#  FINAL RETRAIN — ALL HISTORICAL DATA (2005-2025)
#
#  once parameters and features are confirmed, retrain
#  using all available historical data including the validation and test sets.
#  This gives the model the most recent and most relevant data before predicting
#  2026, which is the correct practice for a production forecasting model.
#
#  Parameters and features are already confirmed — no tuning is done here.
#  Early stopping uses 2024-2025 as a mini-validation window (most recent 2
#  seasons) to determine the optimal number of trees, then the final model
#  trains on the full 2005-2025 dataset.
#
#  Two-phase approach:
#    Phase 1: Train on 2005-2023, validate on 2024-2025 — find best tree count
#    Phase 2: Train on 2005-2025 using that tree count (fixed iterations)
#
#  Settings below reflect the 02a_grid_search run completed 2026-07-31
#  (train<=2022 / val 2023-24 / test 2025 cycle):
#  RS: depth=5, lr=0.08, l2=5  — SHAP-selected features (see opt_features_runs_scored.txt)
#  RA: depth=4, lr=0.08, l2=3  — SHAP-selected features (see opt_features_runs_allowed.txt)
#  WL: depth=3, lr=0.08, l2=3  — SHAP-selected features (see opt_features_win_loss.txt)


# 0. LOAD
df <- read.csv("../data/model_features.csv",
               stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)

# 1. SPLITS
# Phase 1 splits — find optimal tree count
phase1_train <- df %>% filter(Season <= 2023)
phase1_val   <- df %>% filter(Season %in% c(2024, 2025))

# Phase 2 — full training set
full_train   <- df %>% filter(Season <= 2025)
pred_df      <- df %>% filter(Season == 2026)



# 2. FEATURE LISTS
cat_features <- c("Team", "Opp")
id_cols      <- c("Team", "Season", "Date", "Opp")
target_cols  <- c("runs_scored", "runs_allowed", "win")

feat_cols_rs <- readLines("../models/opt_features_runs_scored.txt")
feat_cols_ra <- readLines("../models/opt_features_runs_allowed.txt")
feat_cols_wl <- readLines("../models/opt_features_win_loss.txt")

# 3. HELPERS
make_pool <- function(data, target_col, feat_cols, cat_features) {
  all_feat_cols <- c(feat_cols, cat_features)
  X <- data[, all_feat_cols]
  y <- data[[target_col]]
  for (col in cat_features) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))
accuracy <- function(actual, prob, threshold = 0.5) {
  mean(as.integer(prob >= threshold) == actual, na.rm = TRUE)
}
logit_to_prob <- function(x) 1 / (1 + exp(-x))

#4. TWO-PHASE TRAINING FUNCTION
train_two_phase <- function(target_col, loss_function, feat_cols,
                            params_list, label, is_classification = FALSE) {

  cat(sprintf("════════════════════════════════════════════════════════\n"))
  cat(sprintf("  %s\n", label))
  cat(sprintf("════════════════════════════════════════════════════════\n"))

  # Phase 1 — find optimal tree count using early stopping
  cat("  Phase 1: Finding optimal tree count (train 2005-2023, val 2024-2025)...\n")

  p1_tr <- make_pool(phase1_train, target_col, feat_cols, cat_features)
  p1_va <- make_pool(phase1_val,   target_col, feat_cols, cat_features)

  params_phase1 <- c(params_list, list(
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    verbose        = 100
  ))

  model_phase1 <- catboost.train(
    learn_pool = p1_tr,
    test_pool  = p1_va,
    params     = params_phase1
  )

  best_trees <- model_phase1$tree_count
  cat(sprintf("  Optimal tree count: %d\n\n", best_trees))

  # Phase 2 — retrain on full 2005-2025 with fixed iterations
  cat(sprintf("  Phase 2: Retraining on full 2005-2025 (%d trees, no early stopping)...\n",
              best_trees))

  # NOTE: must use modifyList (not c()) here — params_list already has an
  # "iterations" entry, and c() on named lists keeps both instead of
  # overwriting, which silently ignored best_trees and trained to the
  # original 2000-iteration ceiling with no early stopping (massive
  # overfitting) the first time this ran.
  params_phase2 <- modifyList(params_list, list(
    iterations     = best_trees,
    use_best_model = FALSE,
    verbose        = 100
  ))

  full_tr <- make_pool(full_train, target_col, feat_cols, cat_features)

  model_final <- catboost.train(
    learn_pool = full_tr,
    params     = params_phase2
  )

  cat(sprintf("  Final model trees: %d\n", model_final$tree_count))

  # Evaluate on 2026 prediction set for a sanity check — skipped until real
  # post-deadline 2026 rows exist in model_features.csv (pred_df has 0 rows
  # until then; building a pool on an empty frame errors out in catboost).
  if (nrow(pred_df) > 0) {
    pred_pool <- make_pool(pred_df, target_col, feat_cols, cat_features)
    preds     <- catboost.predict(model_final, pred_pool)
    if (is_classification) preds <- logit_to_prob(preds)

    cat(sprintf("  2026 prediction range: %.2f to %.2f\n\n",
                min(preds), max(preds)))
  } else {
    cat("  No 2026 post-deadline rows yet — skipping prediction sanity check.\n\n")
  }

  return(model_final)
}

#5. RUNS SCORED
model_rs <- train_two_phase(
  target_col        = "runs_scored",
  loss_function     = "RMSE",
  feat_cols         = feat_cols_rs,
  params_list       = list(loss_function = "RMSE", iterations = 2000,
                           learning_rate = 0.08, depth = 5, l2_leaf_reg = 5,
                           random_seed = 42),
  label             = sprintf("Runs Scored — depth=5, lr=0.08, l2=5, features=%d",
                              length(feat_cols_rs)),
  is_classification = FALSE
)

catboost.save_model(model_rs,
                    "../models/model_runs_scored.cbm")
cat("Saved model_runs_scored.cbm\n\n")

#6. RUNS ALLOWED
model_ra <- train_two_phase(
  target_col        = "runs_allowed",
  loss_function     = "RMSE",
  feat_cols         = feat_cols_ra,
  params_list       = list(loss_function = "RMSE", iterations = 2000,
                           learning_rate = 0.08, depth = 4, l2_leaf_reg = 3,
                           random_seed = 42),
  label             = sprintf("Runs Allowed — depth=4, lr=0.08, l2=3, features=%d",
                              length(feat_cols_ra)),
  is_classification = FALSE
)

catboost.save_model(model_ra,
                    "../models/model_runs_allowed.cbm")

# 7. WIN / LOSS
model_wl <- train_two_phase(
  target_col        = "win",
  loss_function     = "Logloss",
  feat_cols         = feat_cols_wl,
  params_list       = list(loss_function = "Logloss", iterations = 2000,
                           learning_rate = 0.08, depth = 3, l2_leaf_reg = 3,
                           random_seed = 42),
  label             = sprintf("Win/Loss — depth=3, lr=0.08, l2=3, features=%d",
                              length(feat_cols_wl)),
  is_classification = TRUE
)

catboost.save_model(model_wl,
                    "../models/model_win_loss.cbm")

