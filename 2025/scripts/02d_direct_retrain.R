library(dplyr)
library(catboost)

# ════════════════════════════════════════════════════════════════════════════════
#  DIRECT RETRAIN — BEST PARAMETERS + BEST FEATURES
#
#  Retrains all three models using confirmed best settings from overnight
#  grid search and SHAP optimization. No searching — straight retrain.
#
#  RS: depth=5, lr=0.08, l2=2  — 50 SHAP-selected features
#  RA: depth=4, lr=0.08, l2=3  — 77 SHAP-selected features
#  WL: depth=5, lr=0.02, l2=5  — all 128 features
#
#  Runtime: ~10-15 minutes
#  Output:  model_runs_scored.cbm
#           model_runs_allowed.cbm
#           model_win_loss.cbm
#           retrain_test_results.csv
# ════════════════════════════════════════════════════════════════════════════════

cat(sprintf("Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ── 0. LOAD ────────────────────────────────────────────────────────────────────
df <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# ── 1. SPLITS ─────────────────────────────────────────────────────────────────
train_df <- df %>% filter(Season <= 2021)
val_df   <- df %>% filter(Season %in% c(2022, 2023))
test_df  <- df %>% filter(Season == 2024)

cat("Train:", nrow(train_df), "| Val:", nrow(val_df), "| Test:", nrow(test_df), "\n\n")

# ── 2. FEATURE LISTS ──────────────────────────────────────────────────────────
cat_features <- c("Team", "Opp")
id_cols      <- c("Team", "Season", "Date", "Opp")
target_cols  <- c("runs_scored", "runs_allowed", "win")

# RS and RA — load exact SHAP-selected ordered feature lists
feat_cols_rs <- readLines("../models/opt_features_runs_scored.txt")
feat_cols_ra <- readLines("../models/opt_features_runs_allowed.txt")

# WL — all features on current matrix
feat_cols_wl <- setdiff(names(df), c(id_cols, target_cols))

cat("RS features:", length(feat_cols_rs), "\n")
cat("RA features:", length(feat_cols_ra), "\n")
cat("WL features:", length(feat_cols_wl), "\n\n")

# ── 3. HELPERS ────────────────────────────────────────────────────────────────
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

# ── 4. RUNS SCORED ────────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════\n")
cat("  Runs Scored — depth=5, lr=0.08, l2=2, features=50\n")
cat("════════════════════════════════════════════════════════\n")

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
    l2_leaf_reg    = 2,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 100
  )
)

rs_val_rmse  <- rmse(val_df$runs_scored,  catboost.predict(model_rs, va_rs))
rs_test_rmse <- rmse(test_df$runs_scored, catboost.predict(model_rs, te_rs))
cat(sprintf("  Val RMSE:  %.4f\n", rs_val_rmse))
cat(sprintf("  Test RMSE: %.4f\n\n", rs_test_rmse))

catboost.save_model(model_rs, "../models/model_runs_scored.cbm")
cat("  Saved model_runs_scored.cbm\n\n")

# ── 5. RUNS ALLOWED ───────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════\n")
cat("  Runs Allowed — depth=4, lr=0.08, l2=3, features=77\n")
cat("════════════════════════════════════════════════════════\n")

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

ra_val_rmse  <- rmse(val_df$runs_allowed,  catboost.predict(model_ra, va_ra))
ra_test_rmse <- rmse(test_df$runs_allowed, catboost.predict(model_ra, te_ra))
cat(sprintf("  Val RMSE:  %.4f\n", ra_val_rmse))
cat(sprintf("  Test RMSE: %.4f\n\n", ra_test_rmse))

catboost.save_model(model_ra, "../models/model_runs_allowed.cbm")
cat("  Saved model_runs_allowed.cbm\n\n")

# ── 6. WIN / LOSS ─────────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════\n")
cat("  Win/Loss — depth=5, lr=0.02, l2=5, features=128\n")
cat("════════════════════════════════════════════════════════\n")

tr_wl <- make_pool(train_df, "win", feat_cols_wl, cat_features)
va_wl <- make_pool(val_df,   "win", feat_cols_wl, cat_features)
te_wl <- make_pool(test_df,  "win", feat_cols_wl, cat_features)

model_wl <- catboost.train(
  learn_pool = tr_wl,
  test_pool  = va_wl,
  params = list(
    loss_function  = "Logloss",
    iterations     = 2000,
    learning_rate  = 0.02,
    depth          = 5,
    l2_leaf_reg    = 5,
    od_type        = "Iter",
    od_wait        = 75,
    use_best_model = TRUE,
    random_seed    = 42,
    verbose        = 100
  )
)

wl_preds_val  <- logit_to_prob(catboost.predict(model_wl, va_wl))
wl_preds_test <- logit_to_prob(catboost.predict(model_wl, te_wl))
wl_val_acc    <- accuracy(val_df$win,  wl_preds_val)
wl_test_acc   <- accuracy(test_df$win, wl_preds_test)
cat(sprintf("  Val accuracy:  %.4f\n", wl_val_acc))
cat(sprintf("  Test accuracy: %.4f\n\n", wl_test_acc))

catboost.save_model(model_wl, "../models/model_win_loss.cbm")
cat("  Saved model_win_loss.cbm\n\n")

# ── 7. SUMMARY ────────────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════\n")
cat("  FINAL RESULTS — Test Set 2024\n")
cat("════════════════════════════════════════════════════════\n")
cat("                      RS RMSE    RA RMSE    WL Acc\n")
cat("  Pythagorean:        3.0730     3.0777     0.5470\n")
cat("  Previous best:      3.0493     3.0504     0.5858\n")
cat(sprintf("  This retrain:       %.4f     %.4f     %.4f\n",
            rs_test_rmse, ra_test_rmse, wl_test_acc))

results <- data.frame(
  model      = c("runs_scored", "runs_allowed", "win_loss"),
  val_score  = c(rs_val_rmse,  ra_val_rmse,  wl_val_acc),
  test_score = c(rs_test_rmse, ra_test_rmse, wl_test_acc)
)
write.csv(results, "../outputs/retrain_test_results.csv", row.names = FALSE)

cat(sprintf("\nFinished: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("\nNext step: run 03_predict_2025.R\n")
