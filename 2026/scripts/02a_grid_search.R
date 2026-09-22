library(dplyr)
library(catboost)

#  EXTENSIVE OPTIMIZATION — HYPERPARAMETER GRID AND SHAP FEATURE SELECTION
#
#  Three phases, run sequentially:
#    Phase 1: Full parameter grid search (36 combos x 3 models x 4 CV folds)
#    Phase 2: SHAP-based feature selection using best params from Phase 1
#    Phase 3: Final retrain with best params + best features, evaluate on 2025
#
#  Each model is optimized independently — separate params and feature subsets.
#  All verbose output is suppressed during grid search for clean progress logs.
#  Checkpoint CSVs saved after each phase so nothing is lost if script crashes.


# 0. LOAD
df <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# 1. SETUP
id_cols      <- c("Team", "Season", "Date", "Opp")
target_cols  <- c("runs_scored", "runs_allowed", "win")
cat_features <- c("Team", "Opp")
all_cols     <- names(df)

train_df <- df %>% filter(Season <= 2022)
val_df   <- df %>% filter(Season %in% c(2023, 2024))
test_df  <- df %>% filter(Season == 2025)

# Model-specific feature sets
rs_exclude <- c(
  id_cols, target_cols,
  grep("^(cum_ER|cum_H_allowed|cum_HR_allowed|cum_BB_allowed|cum_SO_pit|cum_HBP_pit|cum_WP|cum_ERA|cum_RA|roll_ERA|roll_SO_pit|roll_BB_allowed|roll_RA|slope_RA|slope_ERA|starter_|bullpen_)",
       all_cols, value = TRUE),
  grep("^opp_(cum_R|cum_H_per|cum_HR_per|cum_BB_per|cum_SO_per|cum_RBI|cum_SB|cum_LOB|cum_OBP|cum_SLG|cum_OPS|cum_BA|cum_XBH|cum_WPA|cum_RE24|cum_run_diff|roll_R|roll_OBP|roll_SLG|roll_OPS|roll_HR|roll_BB_per|roll_SO_per|roll_win|roll_run_diff|slope_R|slope_win|slope_OPS|slope_run|q25_R|q75_R|iqr_R|q25_run|q75_run|iqr_run|cv_run)",
       all_cols, value = TRUE)
)
feat_cols_rs <- setdiff(all_cols, rs_exclude)

ra_exclude <- c(
  id_cols, target_cols,
  grep("^(cum_R_per|cum_H_per|cum_HR_per|cum_BB_per|cum_SO_per|cum_RBI|cum_SB|cum_LOB|cum_OBP|cum_SLG|cum_OPS|cum_BA|cum_XBH|cum_WPA|cum_RE24|roll_R_per|roll_OBP|roll_SLG|roll_OPS|roll_HR|roll_BB_per|roll_SO_per|slope_R|slope_OPS|q25_R_per|q75_R_per|iqr_R_per)",
       all_cols, value = TRUE),
  grep("^opp_(cum_ER|cum_H_allowed|cum_HR_allowed|cum_BB_allowed|cum_SO_pit|cum_HBP_pit|cum_WP|cum_ERA|cum_RA|roll_ERA|roll_SO_pit|roll_BB_allowed|roll_RA|slope_RA|slope_ERA|starter_|bullpen_)",
       all_cols, value = TRUE)
)

feat_cols_ra <- setdiff(all_cols, ra_exclude)

feat_cols_wl <- setdiff(all_cols, c(id_cols, target_cols))

cat("Runs Scored features:", length(feat_cols_rs), "\n")
cat("Runs Allowed features:", length(feat_cols_ra), "\n")
cat("Win/Loss features:", length(feat_cols_wl), "\n")
cat("Train:", nrow(train_df), "| Val:", nrow(val_df), "| Test:", nrow(test_df), "\n\n")

#2. HELPERS
make_pool <- function(data, target_col, feat_cols, cat_features) {
  all_feat_cols <- c(feat_cols, cat_features)
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

rolling_folds <- list(
  list(train_end = 2019, val_year = 2020),
  list(train_end = 2020, val_year = 2021),
  list(train_end = 2021, val_year = 2022),
  list(train_end = 2022, val_year = 2023)
)

# 3. PARAMETER GRID
# Expanded grid for overnight run — time is not a factor
param_grid <- expand.grid(
  depth         = c(3, 4, 5, 6, 7),
  learning_rate = c(0.01, 0.02, 0.03, 0.05, 0.08),
  l2_leaf_reg   = c(1, 2, 3, 5, 8, 10, 15),
  stringsAsFactors = FALSE
)

cat(sprintf("Parameter grid: %d combinations\n", nrow(param_grid)))
cat(sprintf("Total CV runs:  %d\n", nrow(param_grid) * 3 * 4))
cat(sprintf("Estimated runtime: ~8-12 hours\n\n"))


#  PHASE 1 — HYPERPARAMETER GRID SEARCH


phase1_grid_search <- function(target_col, loss_function, feat_cols,
                               is_classification = FALSE, label = target_col) {

  cat(sprintf("%s\n", strrep("─", 60)))
  cat(sprintf("  PHASE 1 | Grid search: %s\n", label))
  cat(sprintf("  Features: %d\n", length(feat_cols)))
  cat(sprintf("  Started: %s\n", format(Sys.time(), "%H:%M:%S")))
  cat(sprintf("%s\n", strrep("─", 60)))

  results <- data.frame()

  for (i in seq_len(nrow(param_grid))) {
    params <- param_grid[i, ]

    cb_params <- list(
      loss_function  = loss_function,
      iterations     = 2000,
      learning_rate  = params$learning_rate,
      depth          = params$depth,
      l2_leaf_reg    = params$l2_leaf_reg,
      od_type        = "Iter",
      od_wait        = 75,
      use_best_model = TRUE,
      random_seed    = 42,
      verbose        = 0
    )

    fold_scores <- numeric(length(rolling_folds))

    for (j in seq_along(rolling_folds)) {
      fold    <- rolling_folds[[j]]
      tr      <- df %>% filter(Season <= fold$train_end)
      va      <- df %>% filter(Season == fold$val_year)
      tr_pool <- make_pool(tr, target_col, feat_cols, cat_features)
      va_pool <- make_pool(va, target_col, feat_cols, cat_features)

      model <- catboost.train(
        learn_pool = tr_pool,
        test_pool  = va_pool,
        params     = cb_params
      )

      preds <- catboost.predict(model, va_pool)
      if (is_classification) preds <- logit_to_prob(preds)

      fold_scores[j] <- if (is_classification) {
        accuracy(va[[target_col]], preds)
      } else {
        rmse(va[[target_col]], preds)
      }
    }

    mean_score <- mean(fold_scores)

    cat(sprintf("  [%2d/%2d] depth=%d lr=%.2f l2=%2d | CV: %.4f\n",
                i, nrow(param_grid),
                params$depth, params$learning_rate, params$l2_leaf_reg,
                mean_score))

    results <- rbind(results, data.frame(
      model         = label,
      combo         = i,
      depth         = params$depth,
      learning_rate = params$learning_rate,
      l2_leaf_reg   = params$l2_leaf_reg,
      cv_score      = mean_score,
      stringsAsFactors = FALSE
    ))
  }

  best_row <- if (is_classification) {
    results[which.max(results$cv_score), ]
  } else {
    results[which.min(results$cv_score), ]
  }

  cat(sprintf("\n  Best: depth=%d lr=%.2f l2=%d | CV: %.4f\n",
              best_row$depth, best_row$learning_rate,
              best_row$l2_leaf_reg, best_row$cv_score))
  cat(sprintf("  Finished: %s\n\n", format(Sys.time(), "%H:%M:%S")))

  return(list(results = results, best = best_row))
}

# Run Phase 1 for all three models
grid_rs <- phase1_grid_search("runs_scored",  "RMSE",    feat_cols_rs, FALSE, "Runs Scored")
grid_ra <- phase1_grid_search("runs_allowed", "RMSE",    feat_cols_ra, FALSE, "Runs Allowed")
grid_wl <- phase1_grid_search("win",          "Logloss", feat_cols_wl, TRUE,  "Win/Loss")

# Save Phase 1 results
all_grid <- bind_rows(grid_rs$results, grid_ra$results, grid_wl$results)
write.csv(all_grid, "../outputs/opt_phase1_grid_results.csv", row.names = FALSE)
cat("Phase 1 complete. Results saved to opt_phase1_grid_results.csv\n\n")


#  PHASE 2 — SHAP FEATURE SELECTION (using best params from Phase 1)

phase2_shap_selection <- function(target_col, loss_function, best_params, feat_cols,
                                  is_classification = FALSE, label = target_col) {

  cat(sprintf("%s\n", strrep("─", 60)))
  cat(sprintf("  PHASE 2 | SHAP feature selection: %s\n", label))
  cat(sprintf("  Using: depth=%d lr=%.2f l2=%d | Features: %d\n",
              best_params$depth, best_params$learning_rate,
              best_params$l2_leaf_reg, length(feat_cols)))
  cat(sprintf("  Started: %s\n", format(Sys.time(), "%H:%M:%S")))
  cat(sprintf("%s\n", strrep("─", 60)))

  cb_params <- list(
    loss_function  = loss_function,
    iterations     = 2000,
    learning_rate  = best_params$learning_rate,
    depth          = best_params$depth,
    l2_leaf_reg    = best_params$l2_leaf_reg,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 0
  )

  # Train baseline model on full training set
  tr_pool <- make_pool(train_df, target_col, feat_cols, cat_features)
  va_pool <- make_pool(val_df,   target_col, feat_cols, cat_features)

  model_base <- catboost.train(
    learn_pool = tr_pool,
    test_pool  = va_pool,
    params     = c(cb_params, list(verbose = 100))
  )

  # Compute SHAP values on validation set
  cat("\n  Computing SHAP values on validation set...\n")
  shap_matrix <- catboost.get_feature_importance(
    model_base, pool = va_pool, type = "ShapValues"
  )

  all_feat_cols   <- c(feat_cols, cat_features)
  shap_matrix     <- shap_matrix[, -ncol(shap_matrix)]
  mean_abs_shap   <- colMeans(abs(shap_matrix))

  shap_df <- data.frame(
    model         = label,
    feature       = all_feat_cols,
    mean_abs_shap = mean_abs_shap,
    stringsAsFactors = FALSE
  ) %>% arrange(desc(mean_abs_shap))

  cat("  Top 10 features by SHAP:\n")
  print(head(shap_df[, c("feature","mean_abs_shap")], 10), row.names = FALSE)

  # Baseline scores
  preds_base_val <- catboost.predict(model_base, va_pool)
  if (is_classification) preds_base_val <- logit_to_prob(preds_base_val)
  score_base_val <- if (is_classification) accuracy(val_df[[target_col]], preds_base_val) else rmse(val_df[[target_col]], preds_base_val)

  te_pool <- make_pool(test_df, target_col, feat_cols, cat_features)
  preds_base_test <- catboost.predict(model_base, te_pool)
  if (is_classification) preds_base_test <- logit_to_prob(preds_base_test)
  score_base_test <- if (is_classification) accuracy(test_df[[target_col]], preds_base_test) else rmse(test_df[[target_col]], preds_base_test)

  cat(sprintf("\n  Baseline (all %d features) | Val: %.4f | Test: %.4f\n",
              length(feat_cols), score_base_val, score_base_test))

  # Test feature count thresholds — expanded for overnight run
  non_cat_shap <- shap_df %>% filter(!feature %in% cat_features)
  thresholds   <- c(5, 8, 10, 12, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 70, 80)
  # Cap at number of available non-cat features
  thresholds   <- thresholds[thresholds <= nrow(non_cat_shap)]
  threshold_results <- data.frame()

  cat("\n  Testing feature count thresholds:\n")

  for (n in thresholds) {
    top_features <- non_cat_shap %>% head(n) %>% pull(feature)

    tr_sub <- make_pool(train_df, target_col, top_features, cat_features)
    va_sub <- make_pool(val_df,   target_col, top_features, cat_features)
    te_sub <- make_pool(test_df,  target_col, top_features, cat_features)

    model_sub <- catboost.train(
      learn_pool = tr_sub,
      test_pool  = va_sub,
      params     = cb_params
    )

    preds_val  <- catboost.predict(model_sub, va_sub)
    preds_test <- catboost.predict(model_sub, te_sub)
    if (is_classification) {
      preds_val  <- logit_to_prob(preds_val)
      preds_test <- logit_to_prob(preds_test)
    }

    acc_val  <- if (is_classification) accuracy(val_df[[target_col]],  preds_val)  else rmse(val_df[[target_col]],  preds_val)
    acc_test <- if (is_classification) accuracy(test_df[[target_col]], preds_test) else rmse(test_df[[target_col]], preds_test)

    cat(sprintf("    Top %2d features | Val: %.4f | Test: %.4f\n",
                n, acc_val, acc_test))

    threshold_results <- rbind(threshold_results, data.frame(
      model        = label,
      n_features   = n,
      val_score    = acc_val,
      test_score   = acc_test,
      stringsAsFactors = FALSE
    ))
  }

  best_n_row <- if (is_classification) {
    threshold_results[which.max(threshold_results$val_score), ]
  } else {
    threshold_results[which.min(threshold_results$val_score), ]
  }

  better_than_baseline <- if (is_classification) {
    best_n_row$val_score > score_base_val
  } else {
    best_n_row$val_score < score_base_val
  }

  if (better_than_baseline) {
    best_n        <- best_n_row$n_features
    best_features <- non_cat_shap %>% head(best_n) %>% pull(feature)
    cat(sprintf("\n  Best subset: top %d features (val: %.4f)\n", best_n, best_n_row$val_score))
  } else {
    best_features <- feat_cols
    best_n        <- length(feat_cols)
    cat(sprintf("\n  Baseline wins — keeping all %d features\n", best_n))
  }

  cat(sprintf("  Finished: %s\n\n", format(Sys.time(), "%H:%M:%S")))

  return(list(
    shap_df           = shap_df,
    threshold_results = threshold_results,
    best_features     = best_features,
    best_n            = best_n,
    baseline_val      = score_base_val,
    baseline_test     = score_base_test
  ))
}

# Run Phase 2 for all three models
shap_rs <- phase2_shap_selection("runs_scored",  "RMSE",    grid_rs$best, feat_cols_rs, FALSE, "Runs Scored")
shap_ra <- phase2_shap_selection("runs_allowed", "RMSE",    grid_ra$best, feat_cols_ra, FALSE, "Runs Allowed")
shap_wl <- phase2_shap_selection("win",          "Logloss", grid_wl$best, feat_cols_wl, TRUE,  "Win/Loss")

# Save Phase 2 results
all_shap       <- bind_rows(shap_rs$shap_df,           shap_ra$shap_df,           shap_wl$shap_df)
all_thresholds <- bind_rows(shap_rs$threshold_results, shap_ra$threshold_results, shap_wl$threshold_results)
write.csv(all_shap,       "../outputs/opt_phase2_shap_rankings.csv",    row.names = FALSE)
write.csv(all_thresholds, "../outputs/opt_phase2_threshold_results.csv", row.names = FALSE)
cat("Phase 2 complete. Results saved.\n\n")


#  PHASE 3 — FINAL RETRAIN WITH BEST PARAMS + BEST FEATURES


cat(sprintf("%s\n", strrep("═", 60)))
cat("  PHASE 3 | Final models with optimal params + features\n")
cat(sprintf("%s\n\n", strrep("═", 60)))

retrain_final <- function(target_col, loss_function, best_params,
                          best_features, is_classification = FALSE,
                          label = target_col) {

  cat(sprintf("── %s ──\n", label))
  cat(sprintf("  Params: depth=%d lr=%.2f l2=%d\n",
              best_params$depth, best_params$learning_rate, best_params$l2_leaf_reg))
  cat(sprintf("  Features: %d\n", length(best_features) + 2))  # +2 for Team/Opp

  cb_params <- list(
    loss_function  = loss_function,
    iterations     = 2000,
    learning_rate  = best_params$learning_rate,
    depth          = best_params$depth,
    l2_leaf_reg    = best_params$l2_leaf_reg,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 100
  )

  tr_pool <- make_pool(train_df, target_col, best_features, cat_features)
  va_pool <- make_pool(val_df,   target_col, best_features, cat_features)
  te_pool <- make_pool(test_df,  target_col, best_features, cat_features)

  model <- catboost.train(
    learn_pool = tr_pool,
    test_pool  = va_pool,
    params     = cb_params
  )

  cat(sprintf("  Trees used: %d\n", model$tree_count))

  preds_val  <- catboost.predict(model, va_pool)
  preds_test <- catboost.predict(model, te_pool)

  if (is_classification) {
    preds_val  <- logit_to_prob(preds_val)
    preds_test <- logit_to_prob(preds_test)
    val_score  <- accuracy(val_df[[target_col]],  preds_val)
    test_score <- accuracy(test_df[[target_col]], preds_test)
    cat(sprintf("  Val accuracy:  %.4f\n", val_score))
    cat(sprintf("  Test accuracy: %.4f\n\n", test_score))
  } else {
    val_score  <- rmse(val_df[[target_col]],  preds_val)
    test_score <- rmse(test_df[[target_col]], preds_test)
    cat(sprintf("  Val RMSE:  %.4f\n", val_score))
    cat(sprintf("  Test RMSE: %.4f\n\n", test_score))
  }

  return(list(model = model, val_score = val_score, test_score = test_score,
              preds_test = preds_test))
}

final_rs <- retrain_final("runs_scored",  "RMSE",    grid_rs$best, shap_rs$best_features, FALSE, "Runs Scored")
final_ra <- retrain_final("runs_allowed", "RMSE",    grid_ra$best, shap_ra$best_features, FALSE, "Runs Allowed")
final_wl <- retrain_final("win",          "Logloss", grid_wl$best, shap_wl$best_features, TRUE,  "Win/Loss")

# FINAL COMPARISON
cat(sprintf("%s\n", strrep("═", 60)))
cat("  FINAL COMPARISON — Test Set (2025)\n")
cat(sprintf("%s\n", strrep("═", 60)))
cat("                          RS RMSE   RA RMSE   WL Acc\n")
# NOTE: the rows below were the benchmark progression from the 2025-forecast
# cycle (test set = 2024) — not directly comparable to this cycle's 2025 test
# set. Kept for historical reference only. Replace with this cycle's own
# progression as results come in.
cat("  Prior cycle (test=2024) Pythagorean baseline: 3.0730  3.0777  0.5470\n")
cat("  Prior cycle (test=2024) Optimized:             3.0524  3.0535  0.5796\n")
cat(sprintf("  Optimized (this run, test=2025):   %.4f    %.4f    %.4f\n",
            final_rs$test_score, final_ra$test_score, final_wl$test_score))

# SAVE FINAL MODELS
# Baselines to beat — best result per model across all previous runs
save_model <- function(model, filename, new_score, prev_score,
                       lower_is_better = TRUE, label = "") {
  improved <- if (lower_is_better) new_score < prev_score else new_score > prev_score
  if (improved) {
    catboost.save_model(model, filename)
    cat(sprintf("  SAVED %s (%.4f beats %.4f)\n", label, new_score, prev_score))
  } else {
    cat(sprintf("  SKIPPED %s (%.4f does not beat %.4f)\n", label, new_score, prev_score))
  }
  return(improved)
}

# Inf/-Inf = no established benchmark yet for the 2025-test / 2026-forecast
# cycle, so the first run always saves. Once you have a real result, replace
# Inf/-Inf below with that run's actual test_score to gate future runs.
cat("\nModel saving:\n")
saved_rs <- save_model(final_rs$model, "../models/model_runs_scored.cbm",
                       final_rs$test_score, Inf,  TRUE,  "Runs Scored")
saved_ra <- save_model(final_ra$model, "../models/model_runs_allowed.cbm",
                       final_ra$test_score, Inf,  TRUE,  "Runs Allowed")
saved_wl <- save_model(final_wl$model, "../models/model_win_loss.cbm",
                       final_wl$test_score, -Inf, FALSE, "Win/Loss")

# Save selected feature lists for any model that improved
if (saved_rs) writeLines(shap_rs$best_features, "../models/opt_features_runs_scored.txt")
if (saved_ra) writeLines(shap_ra$best_features, "../models/opt_features_runs_allowed.txt")
if (saved_wl) writeLines(shap_wl$best_features, "../models/opt_features_win_loss.txt")

# Save test predictions with optimized models
test_preds <- test_df %>%
  select(Team, Season, Date, Opp, runs_scored, runs_allowed, win) %>%
  mutate(
    pred_runs_scored  = final_rs$preds_test,
    pred_runs_allowed = final_ra$preds_test,
    pred_win_prob     = final_wl$preds_test
  )

write.csv(test_preds, "../outputs/opt_test_predictions.csv", row.names = FALSE)

