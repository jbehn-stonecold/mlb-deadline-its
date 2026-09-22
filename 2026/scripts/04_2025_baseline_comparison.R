library(dplyr)
library(ggplot2)
library(patchwork)


#  2025 MODEL vs PYTHAGOREAN BASELINE — TEAM IMPACT COMPARISON
#
#  Shows how well the model predicted each team's 2025 second-half wins,
#  RS/G, and RA/G relative to the Pythagorean naive baseline.
#  Includes paired t-test results (RS/RA) and McNemar test (W/L).
#
#  Input: 02b_test_results.csv (held-out test set from 02b_direct_retrain,
#  Season == 2025 for the 2026-forecast cycle), model_features.csv

test25 <- read.csv("../outputs/02b_test_results.csv", stringsAsFactors = FALSE)
feats  <- read.csv("../data/model_features.csv",   stringsAsFactors = FALSE)

# Merge to get pythagorean baseline columns for 2025
pyth_cols <- c("Team", "Season", "Date", "cum_R_per_game", "cum_RA_per_game",
               "cum_win_rate", "cum_run_diff_pg")
feats25 <- feats %>%
  filter(Season == 2025) %>%
  select(all_of(pyth_cols)) %>%
  mutate(Date = as.character(Date))

# Add row index before merge to handle doubleheaders
# (same Team+Date appears twice on doubleheader days with identical
# pre-deadline feature values, causing a many-to-many join blowup)
test25 <- test25 %>%
  mutate(Date = as.character(Date), .row = row_number()) %>%
  left_join(feats25, by = c("Team", "Season", "Date")) %>%
  distinct(.row, .keep_all = TRUE) %>%
  select(-.row)

# STATISTICAL TESTS
rmse_fn <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))

# Paired t-test on squared errors: model vs pythagorean (one-sided: model < pyth)
se_rs_model <- (test25$runs_scored  - test25$pred_runs_scored)^2
se_ra_model <- (test25$runs_allowed - test25$pred_runs_allowed)^2
se_rs_pyth  <- (test25$runs_scored  - test25$cum_R_per_game)^2
se_ra_pyth  <- (test25$runs_allowed - test25$cum_RA_per_game)^2

t_rs <- t.test(se_rs_model, se_rs_pyth, paired = TRUE, alternative = "less")
t_ra <- t.test(se_ra_model, se_ra_pyth, paired = TRUE, alternative = "less")

# McNemar test for W/L
model_correct <- as.integer((test25$pred_win_prob >= 0.5) == test25$win)
pyth_pred     <- as.integer(test25$cum_R_per_game > test25$cum_RA_per_game)
pyth_correct  <- as.integer(pyth_pred == test25$win)

# Contingency table: rows = model correct, cols = pyth correct
ct <- table(model_correct, pyth_correct)
# McNemar statistic
b <- ct[2, 1]; c_val <- ct[1, 2]
mn_stat <- (abs(b - c_val) - 1)^2 / (b + c_val)
mn_p    <- pchisq(mn_stat, df = 1, lower.tail = FALSE)

stars_fn <- function(p) {
  if (p <= 0.01) "***" else if (p <= 0.05) "**" else if (p <= 0.10) "*" else ""
}

cat(sprintf("\nPaired t-test RS: t=%.4f, p=%.6f %s\n", t_rs$statistic, t_rs$p.value, stars_fn(t_rs$p.value)))
cat(sprintf("Paired t-test RA: t=%.4f, p=%.6f %s\n", t_ra$statistic, t_ra$p.value, stars_fn(t_ra$p.value)))
cat(sprintf("McNemar W/L:      stat=%.4f, p=%.6f %s\n", mn_stat, mn_p, stars_fn(mn_p)))

# TEAM-LEVEL METRICS
RMSE_RS <- rmse_fn(test25$runs_scored,  test25$pred_runs_scored)
RMSE_RA <- rmse_fn(test25$runs_allowed, test25$pred_runs_allowed)
Z90 <- 1.645; Z95 <- 1.960; Z99 <- 2.576

team25 <- test25 %>%
  group_by(Team) %>%
  summarise(
    games_played       = n(),
    actual_wins        = sum(win, na.rm = TRUE),
    actual_rs_pg       = round(mean(runs_scored, na.rm = TRUE), 2),
    actual_ra_pg       = round(mean(runs_allowed, na.rm = TRUE), 2),

    # Model projections
    proj_wins_model    = round(sum(pred_win_prob, na.rm = TRUE)),
    proj_rs_model      = round(mean(pred_runs_scored, na.rm = TRUE), 2),
    proj_ra_model      = round(mean(pred_runs_allowed, na.rm = TRUE), 2),

    # Pythagorean projections (first-half cumulative)
    proj_wins_pyth     = round(sum(as.integer(cum_R_per_game > cum_RA_per_game),
                                   na.rm = TRUE)),
    proj_rs_pyth       = round(mean(cum_R_per_game, na.rm = TRUE), 2),
    proj_ra_pyth       = round(mean(cum_RA_per_game, na.rm = TRUE), 2),

    # Impacts
    impact_wins_model  = actual_wins - proj_wins_model,
    impact_wins_pyth   = actual_wins - proj_wins_pyth,
    impact_rs_model    = round(actual_rs_pg - proj_rs_model, 2),
    impact_ra_model    = round(actual_ra_pg - proj_ra_model, 2),

    # Model CI (90% — Bernoulli for wins, RMSE/sqrt(n) for RS/RA)
    wins_sd            = round(sqrt(sum(pred_win_prob * (1 - pred_win_prob),
                                        na.rm = TRUE)), 2),
    wins_ci_lo         = round(proj_wins_model - Z90 * wins_sd),
    wins_ci_hi         = round(proj_wins_model + Z90 * wins_sd),
    rs_ci_margin       = round(Z90 * RMSE_RS / sqrt(games_played), 2),
    ra_ci_margin       = round(Z90 * RMSE_RA / sqrt(games_played), 2),

    .groups = "drop"
  ) %>%
  mutate(
    # Impact CI bounds on the model impact
    impact_wins_ci_lo  = actual_wins - wins_ci_hi,
    impact_wins_ci_hi  = actual_wins - wins_ci_lo,
    impact_rs_ci_lo    = round(actual_rs_pg - (proj_rs_model + rs_ci_margin), 2),
    impact_rs_ci_hi    = round(actual_rs_pg - (proj_rs_model - rs_ci_margin), 2),
    impact_ra_ci_lo    = round(actual_ra_pg - (proj_ra_model + ra_ci_margin), 2),
    impact_ra_ci_hi    = round(actual_ra_pg - (proj_ra_model - ra_ci_margin), 2),

    # Forecast coverage stars — how close was actual to projection?
    # *** within 90% CI (best)  ** within 95%  * within 99%  (blank) outside 99%
    wins_stars = case_when(
      abs(impact_wins_model) <= Z90 * wins_sd ~ "***",
      abs(impact_wins_model) <= Z95 * wins_sd ~ "**",
      abs(impact_wins_model) <= Z99 * wins_sd ~ "*",
      TRUE ~ ""
    ),
    rs_se = rs_ci_margin / Z90,
    ra_se = ra_ci_margin / Z90,
    rs_stars = case_when(
      abs(impact_rs_model) <= Z90 * rs_se ~ "***",
      abs(impact_rs_model) <= Z95 * rs_se ~ "**",
      abs(impact_rs_model) <= Z99 * rs_se ~ "*",
      TRUE ~ ""
    ),
    ra_stars = case_when(
      abs(impact_ra_model) <= Z90 * ra_se ~ "***",
      abs(impact_ra_model) <= Z95 * ra_se ~ "**",
      abs(impact_ra_model) <= Z99 * ra_se ~ "*",
      TRUE ~ ""
    ),

    win_label = paste0(ifelse(impact_wins_model >= 0, "+", ""), impact_wins_model,
                       ifelse(wins_stars != "", paste0(" ", wins_stars), "")),
    rs_label  = paste0(sprintf("%+.2f", impact_rs_model),
                       ifelse(rs_stars != "", paste0(" ", rs_stars), "")),
    ra_label  = paste0(sprintf("%+.2f", impact_ra_model),
                       ifelse(ra_stars != "", paste0(" ", ra_stars), "")),

    win_ci_label = sprintf("(%d, %d)", impact_wins_ci_lo, impact_wins_ci_hi),
    rs_ci_label  = sprintf("(%.2f, %.2f)", impact_rs_ci_lo, impact_rs_ci_hi),
    ra_ci_label  = sprintf("(%.2f, %.2f)", impact_ra_ci_lo, impact_ra_ci_hi),

    win_color = ifelse(impact_wins_model > 0, "above",
                       ifelse(impact_wins_model < 0, "below", "zero")),
    rs_color  = ifelse(impact_rs_model > 0, "above",
                       ifelse(impact_rs_model < 0, "below", "zero")),
    ra_color  = ifelse(impact_ra_model < 0, "above",
                       ifelse(impact_ra_model > 0, "below", "zero"))
  ) %>%
  arrange(desc(impact_wins_model)) %>%
  mutate(Team = factor(Team, levels = rev(Team)))

# COLORS AND THEME
col_above <- "#1D9E75"; col_below <- "#D85A30"
col_zero  <- "#B4B2A9"; col_ci    <- "#888780"; col_grid  <- "#EEEEEE"

scale_impact <- scale_fill_manual(
  values = c("above" = col_above, "below" = col_below, "zero" = col_zero),
  guide  = "none"
)

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(color = "#222222", size = 10.5),
    axis.text.x        = element_text(color = "#666666", size = 9),
    axis.title.x       = element_text(color = "#444444", size = 10, margin = margin(t = 6)),
    axis.title.y       = element_blank(),
    plot.title         = element_text(color = "#111111", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(color = "#666666", size = 9, hjust = 0.5,
                                      margin = margin(b = 4)),
    plot.margin        = margin(t = 8, r = 12, b = 8, l = 4)
  )

make_panel <- function(data, x_col, label_col, ci_col, color_col,
                       x_lim_pad, x_breaks, x_labels = NULL,
                       title_str, subtitle_str, x_title,
                       ci_x_col = NULL, hide_y = FALSE) {
  x_lim <- max(abs(data[[x_col]])) + x_lim_pad
  p <- ggplot(data, aes_string(x = x_col, y = "Team", fill = color_col)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
    geom_text(aes_string(label = label_col,
                         hjust = sprintf("ifelse(%s >= 0, -0.15, 1.15)", x_col)),
              size = 2.8, color = "#333333") +
    geom_text(aes_string(label = ci_col,
                         x = sprintf("ifelse(%s >= 0, -0.15, 0.15)", x_col),
                         hjust = sprintf("ifelse(%s >= 0, 1, 0)", x_col)),
              size = 2.2, color = col_ci, fontface = "italic") +
    scale_impact +
    scale_x_continuous(limits = c(-x_lim - x_lim_pad, x_lim + x_lim_pad),
                       breaks = x_breaks,
                       labels = if (!is.null(x_labels)) x_labels else waiver(),
                       expand = expansion(mult = c(0.02, 0.02))) +
    labs(title = title_str, subtitle = subtitle_str, x = x_title) +
    base_theme
  if (hide_y) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  p
}

p_wins <- make_panel(team25, "impact_wins_model", "win_label", "win_ci_label",
                     "win_color", 2, seq(-10, 10, by = 2),
                     title_str    = "Wins vs projection (model)",
                     subtitle_str = "90% CI  |  Stars = significance",
                     x_title      = "Wins above/below projected")

p_rs <- make_panel(team25, "impact_rs_model", "rs_label", "rs_ci_label",
                   "rs_color", 0.3, seq(-1.4, 1.4, by = 0.4),
                   x_labels     = function(x) sprintf("%+.1f", x),
                   title_str    = "Runs scored/game vs projection",
                   subtitle_str = "90% CI  |  Stars = significance",
                   x_title      = "R/G above/below projected",
                   hide_y       = TRUE)

p_ra <- make_panel(team25, "impact_ra_model", "ra_label", "ra_ci_label",
                   "ra_color", 0.3, seq(-1.4, 1.6, by = 0.4),
                   x_labels     = function(x) sprintf("%+.1f", x),
                   title_str    = "Runs allowed/game vs projection",
                   subtitle_str = "90% CI  |  Stars = significance",
                   x_title      = "RA/G above/below projected",
                   hide_y       = TRUE)

# STATISTICAL TEST ANNOTATION
test_label <- sprintf(
  paste0("Model vs Pythagorean baseline — 2025 held-out test set (n=%d games)\n",
         "RS paired t-test: t=%.3f, p=%.4f %s ",
         "RA paired t-test: t=%.3f, p=%.4f %s ",
         "W/L McNemar test: \u03c7\u00b2=%.3f, p=%.4f %s\n",
         "Model RMSE — RS: %.4f  RA: %.4f  |  Pythagorean RMSE — RS: %.4f  RA: %.4f"),
  nrow(test25),
  t_rs$statistic, t_rs$p.value, stars_fn(t_rs$p.value),
  t_ra$statistic, t_ra$p.value, stars_fn(t_ra$p.value),
  mn_stat, mn_p, stars_fn(mn_p),
  rmse_fn(test25$runs_scored, test25$pred_runs_scored),
  rmse_fn(test25$runs_allowed, test25$pred_runs_allowed),
  rmse_fn(test25$runs_scored, test25$cum_R_per_game),
  rmse_fn(test25$runs_allowed, test25$cum_RA_per_game)
)

p_stats <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = test_label,
           hjust = 0.5, vjust = 0.5, size = 3.4,
           color = "#333333", lineheight = 1.7, fontface = "plain") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F4F9F4", color = "#1D9E75",
                                   linewidth = 0.8),
    plot.margin     = margin(1, 14, 1, 14)
  ) +
  xlim(0, 1) + ylim(0, 1)

# COMBINE
top <- (p_wins | p_rs | p_ra) + plot_layout(widths = c(1.2, 1, 1))
final <- (top / p_stats) +
  plot_layout(heights = c(10, 1.4)) +
  plot_annotation(
    title    = "2025 Model Performance — CatBoost vs Pythagorean Baseline",
    subtitle = "Held-out test set  |  Impact = actual second-half result minus model projection",
    caption  = paste0(
      "90% CI shown as (lo, hi) in italics.  Stars: * p\u226410%  ** p\u22645%  *** p\u22641%\n",
      "Paired t-test compares model squared errors vs Pythagorean squared errors per game (one-sided: model < baseline).\n",
      "McNemar test compares per-game W/L correctness between model and baseline."
    )
  )

ggsave("../model-validation/2025_baseline_comparison.png", final,
       width = 16, height = 11, dpi = 200, bg = "white")
