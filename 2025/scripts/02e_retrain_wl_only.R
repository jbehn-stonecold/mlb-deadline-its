library(dplyr)
library(catboost)

# ════════════════════════════════════════════════════════════════════════════════
#  WL RETRAIN ONLY — PITCHER FEATURES EXCLUDED
#
#  Retrains only the Win/Loss model excluding pitcher split features
#  which were found to add noise. Returns WL to its best 114-feature
#  configuration while keeping RS and RA models untouched.
#
#  Parameters: depth=5, lr=0.02, l2=5 (confirmed best from grid search)
#  Runtime: ~2 minutes
#  Output:  model_win_loss.cbm (overwrites existing)
# ════════════════════════════════════════════════════════════════════════════════

cat(sprintf("Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ── 0. LOAD ────────────────────────────────────────────────────────────────────
df <- read.csv("../data/model_features.csv",
               stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
cat("Loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# ── 1. SPLITS ─────────────────────────────────────────────────────────────────
train_df <- df %>% filter(Season <= 2021)
val_df   <- df %>% filter(Season %in% c(2022, 2023))
test_df  <- df %>% filter(Season == 2024)
cat("Train:", nrow(train_df), "| Val:", nrow(val_df), "| Test:", nrow(test_df), "\n\n")

# ── 2. FEATURE LIST ───────────────────────────────────────────────────────────
cat_features <- c("Team", "Opp")
id_cols      <- c("Team", "Season", "Date", "Opp")
target_cols  <- c("runs_scored", "runs_allowed", "win")

# Exclude pitcher split features — these 14 columns add noise to WL
# and dropping them returns the model to its best 114-feature configuration
pitcher_cols <- c(
  "starter_IP_total", "starter_ERA", "starter_IP_per_game", "starter_pct_IP",
  "bullpen_IP_total", "bullpen_ERA", "bullpen_IP_per_game",
  "opp_starter_IP_total", "opp_starter_ERA", "opp_starter_IP_per_game",
  "opp_starter_pct_IP", "opp_bullpen_IP_total", "opp_bullpen_ERA",
  "opp_bullpen_IP_per_game"
)

feat_cols_wl <- setdiff(names(df), c(id_cols, target_cols, pitcher_cols))
cat("WL features:", length(feat_cols_wl), "\n\n")

# ── 3. HELPERS ────────────────────────────────────────────────────────────────
make_pool <- function(data, target_col, feat_cols, cat_features) {
  all_feat_cols <- c(feat_cols, cat_features)
  X <- data[, all_feat_cols]
  y <- data[[target_col]]
  for (col in cat_features) X[[col]] <- as.factor(X[[col]])
  catboost.load_pool(data = X, label = y)
}

accuracy <- function(actual, prob, threshold = 0.5) {
  mean(as.integer(prob >= threshold) == actual, na.rm = TRUE)
}
logit_to_prob <- function(x) 1 / (1 + exp(-x))

# ── 4. WIN / LOSS ─────────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════\n")
cat(sprintf("  Win/Loss — depth=5, lr=0.02, l2=5, features=%d\n",
            length(feat_cols_wl)))
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
    learning_rate  = 0.05,
    depth          = 6,
    l2_leaf_reg    = 3,
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

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
catboost.save_model(model_wl,
                    "../models/model_win_loss.cbm")
cat("  Saved model_win_loss.cbm\n\n")

# ── 6. SUMMARY ────────────────────────────────────────────────────────────────
cat("════════════════════════════════════════════════════════\n")
cat("  RESULTS — Test Set 2024\n")
cat("════════════════════════════════════════════════════════\n")
cat("  Pythagorean baseline:  0.5470\n")
cat("  Previous best:         0.5858\n")
cat(sprintf("  This retrain:          %.4f\n", wl_test_acc))

cat(sprintf("\nFinished: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("\nNext step: run 03_predict_2025.R\n")
