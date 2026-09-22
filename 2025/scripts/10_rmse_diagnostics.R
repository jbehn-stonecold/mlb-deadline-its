library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)

# ════════════════════════════════════════════════════════════════════════════════
#  RMSE DIAGNOSTIC VISUALIZATIONS
#
#  Four panels showing what the RMSE numbers actually mean:
#    1. Residual distributions — 2024 test vs 2025 out-of-sample
#    2. Actual vs predicted scatter — 2024 and 2025 side by side
#    3. Rolling 15-game absolute error through 2025 second half
#    4. Total absolute error per team — model accuracy by franchise
#
#  Input:  test_predictions.csv
#          predictions_2025_game_level.csv
#          predictions_2025_team_summary.csv
#  Output: rmse_diagnostics.png
# ════════════════════════════════════════════════════════════════════════════════

test24  <- read.csv("../outputs/test_predictions.csv",             stringsAsFactors = FALSE)
game25  <- read.csv("../outputs/predictions_2025_game_level.csv",  stringsAsFactors = FALSE)
team25  <- read.csv("../outputs/predictions_2025_team_summary.csv", stringsAsFactors = FALSE)

test24$Date  <- as.Date(test24$Date)
game25$Date  <- as.Date(game25$Date)

# Residuals
test24 <- test24 %>%
  mutate(
    resid_rs = runs_scored  - pred_runs_scored,
    resid_ra = runs_allowed - pred_runs_allowed,
    context  = "2024 held-out test"
  )

game25 <- game25 %>%
  mutate(
    resid_rs = runs_scored  - pred_runs_scored,
    resid_ra = runs_allowed - pred_runs_allowed,
    context  = "2025 out-of-sample"
  )

rmse_fn <- function(a, p) round(sqrt(mean((a - p)^2, na.rm = TRUE)), 4)

rmse_24_rs <- rmse_fn(test24$runs_scored,  test24$pred_runs_scored)
rmse_24_ra <- rmse_fn(test24$runs_allowed, test24$pred_runs_allowed)
rmse_25_rs <- rmse_fn(game25$runs_scored,  game25$pred_runs_scored)
rmse_25_ra <- rmse_fn(game25$runs_allowed, game25$pred_runs_allowed)

# Colors and theme
col_24   <- "#378ADD"
col_25   <- "#D85A30"
col_rs   <- "#1D9E75"
col_ra   <- "#7F77DD"
col_grid <- "#EEEEEE"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#444444", size = 9.5),
    axis.title       = element_text(color = "#333333", size = 10),
    plot.title       = element_text(color = "#111111", size = 11,
                                    face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(color = "#666666", size = 9,
                                    hjust = 0.5, margin = margin(b = 6)),
    legend.position  = "top",
    legend.text      = element_text(size = 9.5, color = "#444444"),
    plot.margin      = margin(t = 10, r = 12, b = 8, l = 10)
  )

# ── PANEL 1 — RESIDUAL DISTRIBUTIONS ─────────────────────────────────────────
# Combine both contexts for RS residuals
resid_df <- bind_rows(
  test24 %>% select(resid_rs, resid_ra, context),
  game25 %>% select(resid_rs, resid_ra, context)
)

# Normal curve overlays based on RMSE
x_seq <- seq(-12, 16, length.out = 300)
norm_24 <- data.frame(x = x_seq,
                      y = dnorm(x_seq, mean = mean(test24$resid_rs),
                                sd = rmse_24_rs),
                      context = "2024 held-out test")
norm_25 <- data.frame(x = x_seq,
                      y = dnorm(x_seq, mean = mean(game25$resid_rs),
                                sd = rmse_25_rs),
                      context = "2025 out-of-sample")
norm_df <- bind_rows(norm_24, norm_25)

p1 <- ggplot(resid_df, aes(x = resid_rs, fill = context, color = context)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1,
                 alpha = 0.35, position = "identity") +
  geom_line(data = norm_df, aes(x = x, y = y, color = context),
            linewidth = 1.0, inherit.aes = FALSE) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.6,
             linetype = "dashed") +
  annotate("text", x = 10, y = 0.13,
           label = sprintf("2024 RMSE: %.4f", rmse_24_rs),
           color = col_24, size = 3.2, hjust = 0, fontface = "bold") +
  annotate("text", x = 10, y = 0.118,
           label = sprintf("2025 RMSE: %.4f", rmse_25_rs),
           color = col_25, size = 3.2, hjust = 0, fontface = "bold") +
  scale_fill_manual(values  = c("2024 held-out test" = col_24,
                                 "2025 out-of-sample" = col_25), name = NULL) +
  scale_color_manual(values = c("2024 held-out test" = col_24,
                                 "2025 out-of-sample" = col_25), name = NULL) +
  scale_x_continuous(breaks = seq(-10, 15, by = 5),
                     limits = c(-10, 17)) +
  labs(title    = "Residual distribution — runs scored",
       subtitle = "Actual minus predicted per game  |  Curve = normal fit at RMSE",
       x        = "Residual (runs)", y = "Density") +
  base_theme

# ── PANEL 2 — ACTUAL VS PREDICTED SCATTER ────────────────────────────────────
scatter_df <- bind_rows(
  test24 %>% mutate(context = sprintf("2024 test  (RMSE = %.4f)", rmse_24_rs)),
  game25 %>% mutate(context = sprintf("2025 OOS   (RMSE = %.4f)", rmse_25_rs))
) %>%
  mutate(context = factor(context,
                           levels = c(sprintf("2024 test  (RMSE = %.4f)", rmse_24_rs),
                                      sprintf("2025 OOS   (RMSE = %.4f)", rmse_25_rs))))

p2 <- ggplot(scatter_df, aes(x = pred_runs_scored, y = runs_scored,
                              color = context)) +
  geom_abline(slope = 1, intercept = 0, color = "#555555",
              linewidth = 0.7, linetype = "dashed") +
  # RMSE band around the diagonal
  geom_abline(slope = 1, intercept =  rmse_24_rs, color = col_24,
              linewidth = 0.4, linetype = "dotted", alpha = 0.7) +
  geom_abline(slope = 1, intercept = -rmse_24_rs, color = col_24,
              linewidth = 0.4, linetype = "dotted", alpha = 0.7) +
  geom_point(alpha = 0.18, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(~context, ncol = 2) +
  scale_color_manual(values = c(col_24, col_25), guide = "none") +
  scale_x_continuous(limits = c(3, 7),  breaks = seq(3, 7, by = 1)) +
  scale_y_continuous(limits = c(0, 21), breaks = seq(0, 20, by = 5)) +
  labs(title    = "Actual vs predicted runs scored",
       subtitle = "Dashed line = perfect prediction  |  Dotted lines = \u00b1RMSE band  |  Blue line = OLS fit",
       x        = "Predicted runs scored", y = "Actual runs scored") +
  base_theme +
  theme(strip.text = element_text(size = 9.5, face = "bold", color = "#333333"))

# ── PANEL 3 — ROLLING ABSOLUTE ERROR THROUGH 2025 ─────────────────────────────
roll_df <- game25 %>%
  arrange(Date) %>%
  mutate(
    abs_err_rs   = abs(resid_rs),
    abs_err_ra   = abs(resid_ra),
    game_num     = row_number(),
    roll_err_rs  = rollmean(abs_err_rs, k = 15, fill = NA, align = "right"),
    roll_err_ra  = rollmean(abs_err_ra, k = 15, fill = NA, align = "right")
  )

# Overall mean absolute error lines for reference
mae_25_rs <- mean(abs(game25$resid_rs), na.rm = TRUE)
mae_25_ra <- mean(abs(game25$resid_ra), na.rm = TRUE)

p3 <- ggplot(roll_df %>% filter(!is.na(roll_err_rs))) +
  geom_hline(yintercept = mae_25_rs, color = col_rs,
             linewidth = 0.6, linetype = "dashed", alpha = 0.7) +
  geom_hline(yintercept = mae_25_ra, color = col_ra,
             linewidth = 0.6, linetype = "dashed", alpha = 0.7) +
  geom_line(aes(x = game_num, y = roll_err_rs, color = "Runs scored"),
            linewidth = 0.9) +
  geom_line(aes(x = game_num, y = roll_err_ra, color = "Runs allowed"),
            linewidth = 0.9) +
  annotate("text", x = max(roll_df$game_num, na.rm=TRUE) - 5,
           y = mae_25_rs + 0.06,
           label = sprintf("MAE = %.2f", mae_25_rs),
           color = col_rs, size = 2.9, hjust = 1) +
  annotate("text", x = max(roll_df$game_num, na.rm=TRUE) - 5,
           y = mae_25_ra - 0.1,
           label = sprintf("MAE = %.2f", mae_25_ra),
           color = col_ra, size = 2.9, hjust = 1) +
  scale_color_manual(values = c("Runs scored" = col_rs,
                                 "Runs allowed" = col_ra), name = NULL) +
  scale_x_continuous(breaks = seq(0, 1600, by = 200)) +
  scale_y_continuous(limits = c(1.5, 4.5), breaks = seq(1.5, 4.5, by = 0.5)) +
  labs(title    = "Rolling 15-game absolute error — 2025 second half",
       subtitle = "All 30 teams combined  |  Dashed lines = season MAE",
       x        = "Game number (all teams combined)", y = "Mean absolute error (runs)") +
  base_theme

# ── PANEL 4 — TOTAL ABSOLUTE ERROR BY TEAM ────────────────────────────────────
team_err <- game25 %>%
  group_by(Team) %>%
  summarise(
    n_games     = n(),
    mae_rs      = round(mean(abs(resid_rs), na.rm = TRUE), 3),
    mae_ra      = round(mean(abs(resid_ra), na.rm = TRUE), 3),
    .groups     = "drop"
  ) %>%
  arrange(mae_rs) %>%
  mutate(Team = factor(Team, levels = Team))

league_mae_rs <- round(mean(abs(game25$resid_rs), na.rm = TRUE), 3)

p4 <- ggplot(team_err, aes(x = mae_rs, y = Team)) +
  geom_col(width = 0.65,
           fill = ifelse(team_err$mae_rs <= league_mae_rs, col_rs, col_25)) +
  geom_vline(xintercept = league_mae_rs, color = "#555555",
             linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = league_mae_rs + 0.02, y = 31,
           label = sprintf("League avg: %.2f", league_mae_rs),
           color = "#555555", size = 2.9, hjust = 0) +
  geom_text(aes(label = sprintf("%.2f", mae_rs)),
            hjust = -0.15, size = 2.6, color = "#333333") +
  scale_x_continuous(limits = c(0, 4.2),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title    = "Per-team MAE — runs scored (2025 second half)",
       subtitle = "Green = below league average error  |  Red = above league average",
       x        = "Mean absolute error per game (runs)", y = NULL) +
  base_theme +
  theme(axis.text.y = element_text(size = 9))

# ── COMBINE ───────────────────────────────────────────────────────────────────
top_row    <- (p1 | p2) + plot_layout(widths = c(1, 1.3))
bottom_row <- (p3 | p4) + plot_layout(widths = c(1.3, 1))

final <- (top_row / bottom_row) +
  plot_annotation(
    title   = "RMSE Diagnostics — CatBoost Trade Deadline Impact Model",
    subtitle = paste0(
      "2024 held-out test set (n=1,596 games) vs 2025 out-of-sample (n=1,590 games)\n",
      "RS RMSE: 3.0524 (2024) | 3.2047 (2025)     RA RMSE: 3.0535 (2024) | 3.2022 (2025)"
    ),
    caption = paste0(
      "Top left: residual histograms with normal curve overlay at RMSE.  ",
      "Top right: actual vs predicted scatter with \u00b1RMSE band and OLS fit.\n",
      "Bottom left: rolling 15-game MAE across all 2025 second-half games combined.  ",
      "Bottom right: per-team MAE for RS predictions."
    )
  )

ggsave("../visuals/rmse_diagnostics.png", final,
       width = 16, height = 12, dpi = 200, bg = "white")
cat("Saved: rmse_diagnostics.png\n")

# Print summary
cat("\n── Residual Summary ──\n")
cat(sprintf("2024 RS residuals: mean=%.3f  SD=%.3f  RMSE=%.4f\n",
            mean(test24$resid_rs), sd(test24$resid_rs), rmse_24_rs))
cat(sprintf("2024 RA residuals: mean=%.3f  SD=%.3f  RMSE=%.4f\n",
            mean(test24$resid_ra), sd(test24$resid_ra), rmse_24_ra))
cat(sprintf("2025 RS residuals: mean=%.3f  SD=%.3f  RMSE=%.4f\n",
            mean(game25$resid_rs), sd(game25$resid_rs), rmse_25_rs))
cat(sprintf("2025 RA residuals: mean=%.3f  SD=%.3f  RMSE=%.4f\n",
            mean(game25$resid_ra), sd(game25$resid_ra), rmse_25_ra))
