library(dplyr)
library(ggplot2)
library(patchwork)

#  VISUAL 3 — WIN/LOSS CONFUSION MATRIX COMPARISON
#  Side-by-side confusion matrices: CatBoost model vs Pythagorean baseline.
#  Plus McNemar's test result annotated clearly.
#  Input:  02b_test_results.csv, model_features.csv
#  Output: confusion_matrix.png


test25 <- read.csv("../outputs/02b_test_results.csv", stringsAsFactors = FALSE)
feats  <- read.csv("../data/model_features.csv",   stringsAsFactors = FALSE)

feats25 <- feats %>%
  filter(Season == 2025) %>%
  mutate(Date = as.character(Date)) %>%
  select(Team, Season, Date, cum_R_per_game, cum_RA_per_game)

# Add row index before merge to handle doubleheaders
# (same Team+Date appears twice on doubleheader days, causing duplicate rows)
test25_idx <- test25 %>% mutate(.row = row_number())

df <- test25_idx %>%
  mutate(Date = as.character(Date)) %>%
  left_join(feats25, by = c("Team", "Season", "Date")) %>%
  distinct(.row, .keep_all = TRUE) %>%  # remove duplicate rows from doubleheaders
  select(-.row) %>%
  mutate(
    pred_model = as.integer(pred_win_prob >= 0.5),
    pred_pyth  = as.integer(cum_R_per_game > cum_RA_per_game),
    actual     = win
  )

n <- nrow(df)
acc_model <- round(mean(df$pred_model == df$actual) * 100, 2)
acc_pyth  <- round(mean(df$pred_pyth  == df$actual) * 100, 2)

# McNemar
model_correct <- as.integer(df$pred_model == df$actual)
pyth_correct  <- as.integer(df$pred_pyth  == df$actual)
b <- sum(model_correct == 0 & pyth_correct == 1)
c <- sum(model_correct == 1 & pyth_correct == 0)
mn_stat <- (abs(b - c) - 1)^2 / (b + c)
mn_p    <- pchisq(mn_stat, df = 1, lower.tail = FALSE)

# Build confusion matrix data frames
make_cm_df <- function(pred, actual, label) {
  tp <- sum(pred == 1 & actual == 1)
  tn <- sum(pred == 0 & actual == 0)
  fp <- sum(pred == 1 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)
  data.frame(
    Predicted = factor(c("Win","Win","Loss","Loss"), levels=c("Win","Loss")),
    Actual    = factor(c("Win","Loss","Win","Loss"), levels=c("Win","Loss")),
    Count     = c(tp, fp, fn, tn),
    Pct       = round(c(tp,fp,fn,tn)/sum(c(tp,fp,fn,tn))*100, 1),
    Type      = c("TP","FP","FN","TN"),
    model     = label
  )
}

cm_model <- make_cm_df(df$pred_model, df$actual, sprintf("CatBoost Model\nAccuracy: %.2f%%", acc_model))
cm_pyth  <- make_cm_df(df$pred_pyth,  df$actual, sprintf("Pythagorean Baseline\nAccuracy: %.2f%%", acc_pyth))

col_correct  <- "#1D9E75"
col_wrong    <- "#D85A30"
col_grid     <- "#EEEEEE"

make_cm_plot <- function(cm_df, title_str) {
  cm_df <- cm_df %>%
    mutate(
      fill_color = ifelse(Type %in% c("TP","TN"), "Correct", "Incorrect"),
      label      = paste0(Count, "\n(", Pct, "%)")
    )
  ggplot(cm_df, aes(x = Predicted, y = Actual, fill = fill_color)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = label), size = 6.5, fontface = "bold", color = "white") +
    geom_text(aes(label = Type), size = 3.8, color = "white",
              vjust = 3.5, fontface = "italic") +
    scale_fill_manual(
      values = c("Correct" = col_correct, "Incorrect" = col_wrong),
      name   = NULL,
      labels = c("Correct prediction", "Incorrect prediction")
    ) +
    scale_x_discrete(position = "top") +
    labs(title    = title_str,
         x        = "Predicted outcome",
         y        = "Actual outcome") +
    theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.grid        = element_blank(),
      axis.text         = element_text(size = 13, face = "bold", color = "#222222"),
      axis.title        = element_text(size = 12, color = "#444444"),
      plot.title        = element_text(size = 14, face = "bold", hjust = 0.5,
                                       color = "#111111", lineheight = 1.3),
      legend.position   = "bottom",
      legend.text       = element_text(size = 11),
      plot.margin       = margin(12, 20, 12, 20)
    )
}

p_model <- make_cm_plot(cm_model, sprintf("CatBoost Model\nAccuracy: %.2f%%", acc_model))
p_pyth  <- make_cm_plot(cm_pyth,  sprintf("Pythagorean Baseline\nAccuracy: %.2f%%", acc_pyth))

# McNemar annotation panel
mn_label <- sprintf(
  paste0("McNemar\u2019s Test\n\n",
         "Model correct, baseline wrong: %d games\n",
         "Baseline correct, model wrong: %d games\n\n",
         "\u03c7\u00b2 = %.3f\n",
         "p = %.4f  **\n\n",
         "The model correctly called %d games\n",
         "that the baseline missed.\n",
         "The baseline correctly called %d games\n",
         "that the model missed.\n\n",
         "Net advantage: +%d games for model."),
  c, b, mn_stat, mn_p, c, b, c - b
)

p_stats <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = mn_label,
           hjust = 0.5, vjust = 0.5, size = 4.2, color = "#222222",
           lineheight = 1.6) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F4F9F4", color = "#1D9E75", linewidth = 1.2),
    plot.margin     = margin(16, 16, 16, 16)
  ) +
  xlim(0, 1) + ylim(0, 1)

final <- (p_model | p_pyth | p_stats) +
  plot_layout(widths = c(1, 1, 0.85)) +
  plot_annotation(
    title    = "Win/Loss Prediction — Confusion Matrix Comparison",
    subtitle = sprintf("2025 Held-Out Test Set  |  n = %d games  |  ** McNemar p \u2264 0.01", n),
    caption  = paste0(
      "TP = True Positive (correctly predicted win)  |  TN = True Negative (correctly predicted loss)\n",
      "FP = False Positive (predicted win, actual loss)  |  FN = False Negative (predicted loss, actual win)\n",
      "McNemar\u2019s test compares the off-diagonal disagreements between classifiers on the same set of games."
    )
  )

ggsave("../model-validation/confusion_matrix.png", final,
       width = 15, height = 7, dpi = 200, bg = "white")

