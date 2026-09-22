library(dplyr)
library(catboost)


#  02b_direct_retrain.R — DIRECT RETRAIN WITH CONFIRMED BEST SETTINGS
#
#  Retrains all three models using confirmed best parameters and SHAP-selected
#  feature lists. No grid search — straight retrain.
#
#  Settings below reflect the 02a_grid_search run completed 2026-07-31
#  (train<=2022 / val 2023-24 / test 2025 cycle):
#  RS: depth=5,  lr=0.08, l2=5  — SHAP-selected features (opt_features_runs_scored.txt)
#  RA: depth=4,  lr=0.08, l2=3  — SHAP-selected features (opt_features_runs_allowed.txt)
#  WL: depth=3,  lr=0.08, l2=3  — SHAP-selected features (opt_features_win_loss.txt)



# 0. LOAD
df <- read.csv("../data/model_features.csv",
               stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# 1. SPLITS
train_df <- df %>% filter(Season <= 2022)
val_df   <- df %>% filter(Season %in% c(2023, 2024))
test_df  <- df %>% filter(Season == 2025)
cat("Train:", nrow(train_df), "| Val:", nrow(val_df), "| Test:", nrow(test_df), "\n\n")

# 2. FEATURE LISTS
id_cols     <- c("Team", "Season", "Date", "Opp")
target_cols <- c("runs_scored", "runs_allowed", "win")
cat_features <- c("Team", "Opp")

# RS, RA, WL: load SHAP-selected feature lists from txt files
feat_cols_rs <- trimws(readLines(
  "../models/opt_features_runs_scored.txt"))
feat_cols_ra <- trimws(readLines(
  "../models/opt_features_runs_allowed.txt"))
feat_cols_wl <- trimws(readLines(
  "../models/opt_features_win_loss.txt"))

# Verify features exist in the loaded data
feat_cols_rs <- feat_cols_rs[feat_cols_rs %in% names(df)]
feat_cols_ra <- feat_cols_ra[feat_cols_ra %in% names(df)]
feat_cols_wl <- feat_cols_wl[feat_cols_wl %in% names(df)]

cat(sprintf("RS features loaded: %d\n", length(feat_cols_rs)))
cat(sprintf("RA features loaded: %d\n", length(feat_cols_ra)))
cat(sprintf("WL features loaded: %d\n", length(feat_cols_wl)))

# 3. HELPERS
make_pool <- function(data, target_col, feat_cols, cat_features) {
  all_cols <- c(feat_cols, cat_features)
  all_cols <- all_cols[all_cols %in% names(data)]
  X <- data[, all_cols]
  y <- data[[target_col]]
  for (col in cat_features) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

accuracy <- function(actual, prob, threshold = 0.5) {
  mean(as.integer(prob >= threshold) == actual, na.rm = TRUE)
}

logit_to_prob <- function(x) 1 / (1 + exp(-x))

# 4. RUNS SCORED

tr_rs <- make_pool(train_df, "runs_scored", feat_cols_rs, cat_features)
va_rs <- make_pool(val_df,   "runs_scored", feat_cols_rs, cat_features)
te_rs <- make_pool(test_df,  "runs_scored", feat_cols_rs, cat_features)

model_rs <- catboost.train(
  learn_pool = tr_rs,
  test_pool  = va_rs,
  params = list(
    loss_function  = "RMSE",
    iterations     = 2000,
    learning_rate  = 0.08,
    depth          = 5,
    l2_leaf_reg    = 5,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 100
  )
)

rs_preds_test <- catboost.predict(model_rs, te_rs)
rs_test_rmse  <- rmse(test_df$runs_scored, rs_preds_test)

catboost.save_model(model_rs,
  "../models/model_runs_scored.cbm")

# 5. RUNS ALLOWED
tr_ra <- make_pool(train_df, "runs_allowed", feat_cols_ra, cat_features)
va_ra <- make_pool(val_df,   "runs_allowed", feat_cols_ra, cat_features)
te_ra <- make_pool(test_df,  "runs_allowed", feat_cols_ra, cat_features)

model_ra <- catboost.train(
  learn_pool = tr_ra,
  test_pool  = va_ra,
  params = list(
    loss_function  = "RMSE",
    iterations     = 2000,
    learning_rate  = 0.08,
    depth          = 4,
    l2_leaf_reg    = 3,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 100
  )
)

ra_preds_test <- catboost.predict(model_ra, te_ra)
ra_test_rmse  <- rmse(test_df$runs_allowed, ra_preds_test)

catboost.save_model(model_ra,
  "../models/model_runs_allowed.cbm")

# 6. WIN / LOSS
tr_wl <- make_pool(train_df, "win", feat_cols_wl, cat_features)
va_wl <- make_pool(val_df,   "win", feat_cols_wl, cat_features)
te_wl <- make_pool(test_df,  "win", feat_cols_wl, cat_features)

model_wl <- catboost.train(
  learn_pool = tr_wl,
  test_pool  = va_wl,
  params = list(
    loss_function  = "Logloss",
    iterations     = 2000,
    learning_rate  = 0.08,
    depth          = 3,
    l2_leaf_reg    = 3,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 100
  )
)

wl_preds_test <- logit_to_prob(catboost.predict(model_wl, te_wl))
wl_test_acc   <- accuracy(test_df$win, wl_preds_test)

catboost.save_model(model_wl,
  "../models/model_win_loss.cbm")


# 7. SAVE TEST PREDICTIONS
results_df <- test_df %>%
  select(Team, Season, Date, Opp, runs_scored, runs_allowed, win) %>%
  mutate(
    pred_runs_scored  = rs_preds_test,
    pred_runs_allowed = ra_preds_test,
    pred_win_prob     = wl_preds_test
  )

write.csv(results_df,
  "../outputs/02b_test_results.csv",
  row.names = FALSE)
