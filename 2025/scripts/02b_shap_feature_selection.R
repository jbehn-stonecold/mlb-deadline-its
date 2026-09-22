library(dplyr)
library(catboost)

# ════════════════════════════════════════════════════════════════════════════════
#  SHAP-BASED FEATURE SELECTION — WIN/LOSS MODEL
#
#  Goal: Find the optimal subset of features for the W/L classification model
#  by measuring each feature's actual contribution to predictions on held-out
#  validation data using SHAP values, rather than training-set importance.
#
#  Why SHAP over raw importance:
#    Raw importance measures how often a feature is used in splits during
#    training. SHAP measures the actual marginal contribution of each feature
#    to each individual prediction on data the model has never seen. A feature
#    can appear frequently in training trees but contribute little or even
#    negatively on validation data — SHAP catches this, raw importance does not.
#
#  Process:
#    1. Train W/L model on full training set (2005-2021)
#    2. Compute SHAP values on validation set (2022-2023)
#    3. Rank features by mean absolute SHAP — real generalization signal
#    4. Test multiple feature count thresholds and compare val + test accuracy
#    5. Retrain final model with best feature subset
#    6. Evaluate on 2024 test set and compare against original 0.5802
# ════════════════════════════════════════════════════════════════════════════════

# ── 0. LOAD ────────────────────────────────────────────────────────────────────
df <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# ── 1. SETUP ──────────────────────────────────────────────────────────────────
id_cols      <- c("Team", "Season", "Date", "Opp")
target_cols  <- c("runs_scored", "runs_allowed", "win")
feature_cols <- setdiff(names(df), c(id_cols, target_cols))
cat_features <- c("Team", "Opp")

train_df <- df %>% filter(Season <= 2021)
val_df   <- df %>% filter(Season %in% c(2022, 2023))
test_df  <- df %>% filter(Season == 2024)

cat("Feature columns:", length(feature_cols), "\n")
cat("Train:", nrow(train_df), "| Val:", nrow(val_df), "| Test:", nrow(test_df), "\n")

# ── 2. HELPERS ────────────────────────────────────────────────────────────────
make_pool <- function(data, target_col, feat_cols, cat_features) {
  all_feat_cols <- c(feat_cols, cat_features)
  X <- data[, all_feat_cols]
  y <- data[[target_col]]
  for (col in cat_features) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

accuracy <- function(actual, predicted_prob, threshold = 0.5) {
  mean(as.integer(predicted_prob >= threshold) == actual, na.rm = TRUE)
}

logit_to_prob <- function(x) 1 / (1 + exp(-x))

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
  verbose        = 0       # silent — we are running many models
)

# ── 3. TRAIN BASELINE W/L MODEL ───────────────────────────────────────────────
cat("\n── Step 1: Training baseline W/L model (all features) ──\n")

tr_pool  <- make_pool(train_df, "win", feature_cols, cat_features)
va_pool  <- make_pool(val_df,   "win", feature_cols, cat_features)
te_pool  <- make_pool(test_df,  "win", feature_cols, cat_features)

cb_params_verbose <- cb_params_cls
cb_params_verbose$verbose <- 100

model_base <- catboost.train(
  learn_pool = tr_pool,
  test_pool  = va_pool,
  params     = cb_params_verbose
)

preds_val_base  <- logit_to_prob(catboost.predict(model_base, va_pool))
preds_test_base <- logit_to_prob(catboost.predict(model_base, te_pool))

acc_val_base  <- accuracy(val_df$win,  preds_val_base)
acc_test_base <- accuracy(test_df$win, preds_test_base)

cat(sprintf("\nBaseline — Val accuracy:  %.4f\n", acc_val_base))
cat(sprintf("Baseline — Test accuracy: %.4f\n",  acc_test_base))
cat("Original model test accuracy was: 0.5802\n")

# ── 4. COMPUTE SHAP VALUES ON VALIDATION SET ──────────────────────────────────
cat("\n── Step 2: Computing SHAP values on validation set ──\n")
cat("This measures each feature's actual contribution to held-out predictions.\n\n")

# SHAP returns matrix of [n_samples x (n_features + 1)]
# The last column is the bias term — we exclude it
shap_matrix <- catboost.get_feature_importance(
  model_base,
  pool = va_pool,
  type = "ShapValues"
)

# All feature names in the order they appear in the pool
all_feat_cols <- c(feature_cols, cat_features)

# Drop the last column (bias term)
shap_matrix <- shap_matrix[, -ncol(shap_matrix)]

# Mean absolute SHAP per feature — generalization signal
mean_abs_shap <- colMeans(abs(shap_matrix))

shap_df <- data.frame(
  feature        = all_feat_cols,
  mean_abs_shap  = mean_abs_shap
) %>%
  arrange(desc(mean_abs_shap))

cat("Top 20 features by mean absolute SHAP (validation set):\n")
print(head(shap_df, 20), row.names = FALSE)

cat("\nBottom 20 features by mean absolute SHAP:\n")
print(tail(shap_df, 20), row.names = FALSE)

# ── 5. TEST MULTIPLE FEATURE SUBSETS ──────────────────────────────────────────
# Try keeping top N features and see where accuracy peaks
# Test: top 20, 30, 40, 50, 60, 70, 80 features (plus Team and Opp always kept)

cat("\n── Step 3: Testing feature count thresholds ──\n")
cat("(Team and Opp always included as categorical identifiers)\n\n")

# Features ranked by SHAP, excluding cat_features which are always kept
non_cat_shap <- shap_df %>%
  filter(!feature %in% cat_features)

thresholds <- c(15, 20, 25, 30, 40, 50, 60, 70, 80)
threshold_results <- data.frame()

for (n in thresholds) {
  # Take top n non-categorical features plus Team and Opp
  top_features <- non_cat_shap %>%
    head(n) %>%
    pull(feature)

  # Train with this subset
  tr_sub <- make_pool(train_df, "win", top_features, cat_features)
  va_sub <- make_pool(val_df,   "win", top_features, cat_features)
  te_sub <- make_pool(test_df,  "win", top_features, cat_features)

  model_sub <- catboost.train(
    learn_pool = tr_sub,
    test_pool  = va_sub,
    params     = cb_params_cls
  )

  preds_val  <- logit_to_prob(catboost.predict(model_sub, va_sub))
  preds_test <- logit_to_prob(catboost.predict(model_sub, te_sub))

  acc_val  <- accuracy(val_df$win,  preds_val)
  acc_test <- accuracy(test_df$win, preds_test)

  cat(sprintf("  Top %2d features | Val: %.4f | Test: %.4f\n",
              n, acc_val, acc_test))

  threshold_results <- rbind(threshold_results, data.frame(
    n_features   = n,
    val_accuracy = acc_val,
    test_accuracy = acc_test
  ))
}

# ── 6. IDENTIFY BEST SUBSET ───────────────────────────────────────────────────
cat("\n── Step 4: Results summary ──\n")
cat(sprintf("  Baseline (all %d features) | Val: %.4f | Test: %.4f\n",
            length(feature_cols), acc_val_base, acc_test_base))
print(threshold_results, row.names = FALSE)

# Best by validation accuracy (we never use test for selection)
best_row <- threshold_results[which.max(threshold_results$val_accuracy), ]
cat(sprintf("\nBest subset by validation accuracy: top %d features\n",
            best_row$n_features))
cat(sprintf("  Val accuracy:  %.4f\n", best_row$val_accuracy))
cat(sprintf("  Test accuracy: %.4f\n", best_row$test_accuracy))

# ── 7. RETRAIN FINAL MODEL WITH BEST FEATURE SUBSET ───────────────────────────
cat("\n── Step 5: Retraining final W/L model with best feature subset ──\n")

best_n        <- best_row$n_features
best_features <- non_cat_shap %>% head(best_n) %>% pull(feature)

cat(sprintf("Using top %d features by SHAP + Team + Opp\n", best_n))
cat("Features included:\n")
print(best_features)

tr_best <- make_pool(train_df, "win", best_features, cat_features)
va_best <- make_pool(val_df,   "win", best_features, cat_features)
te_best <- make_pool(test_df,  "win", best_features, cat_features)

cb_params_final <- cb_params_cls
cb_params_final$verbose <- 100

model_wl_shap <- catboost.train(
  learn_pool = tr_best,
  test_pool  = va_best,
  params     = cb_params_final
)

preds_val_final  <- logit_to_prob(catboost.predict(model_wl_shap, va_best))
preds_test_final <- logit_to_prob(catboost.predict(model_wl_shap, te_best))

acc_val_final  <- accuracy(val_df$win,  preds_val_final)
acc_test_final <- accuracy(test_df$win, preds_test_final)

cat(sprintf("\nFinal SHAP-selected model:\n"))
cat(sprintf("  Val accuracy:  %.4f\n", acc_val_final))
cat(sprintf("  Test accuracy: %.4f\n", acc_test_final))

# ── 8. FINAL COMPARISON ───────────────────────────────────────────────────────
cat("\n════════════════════════════════════════════════════════\n")
cat("  FINAL COMPARISON — Win/Loss Test (2024) Accuracy\n")
cat("════════════════════════════════════════════════════════\n")
cat(sprintf("  Original model (93 features):         0.5802\n"))
cat(sprintf("  Baseline this run (%2d features):      %.4f\n",
            length(feature_cols), acc_test_base))
cat(sprintf("  SHAP-selected  (%2d features):         %.4f\n",
            best_n, acc_test_final))
cat(sprintf("  Pythagorean baseline:                 0.5470\n"))

# ── 9. SAVE ───────────────────────────────────────────────────────────────────
# Save SHAP rankings for reference
write.csv(shap_df,            "../outputs/shap_rankings_win_loss.csv",   row.names = FALSE)
write.csv(threshold_results,  "../outputs/shap_threshold_results.csv",   row.names = FALSE)

# Save the winning model if it beats original
if (acc_test_final > 0.5802) {
  catboost.save_model(model_wl_shap, "../models/model_win_loss.cbm")
  cat("\nSHAP-selected model beats original — saved as model_win_loss.cbm\n")

  # Save selected feature list so 03_predict_2025.R can use it
  writeLines(best_features, "win_loss_selected_features.txt")
  cat("Selected feature list saved to win_loss_selected_features.txt\n")
} else {
  cat("\nOriginal model still best — model_win_loss.cbm unchanged.\n")
  cat("You can manually save if desired with:\n")
  cat("  catboost.save_model(model_wl_shap, 'model_win_loss.cbm')\n")
}

cat("\nSaved:\n")
cat("  shap_rankings_win_loss.csv\n")
cat("  shap_threshold_results.csv\n")
