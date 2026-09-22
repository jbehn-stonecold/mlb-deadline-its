library(dplyr)
library(ggplot2)
library(patchwork)

# ════════════════════════════════════════════════════════════════════════════════
#  SHAP FEATURE IMPORTANCE VISUALIZATION
#
#  Shows top 15 features by mean absolute SHAP value for each model.
#  Features colored by category to show what type of information drives
#  each model's predictions.
#
#  Input:  opt_phase2_shap_rankings.csv
#  Output: shap_importance.png
# ════════════════════════════════════════════════════════════════════════════════

df <- read.csv("../outputs/opt_phase2_shap_rankings.csv", stringsAsFactors = FALSE)

# ── COLOR CATEGORIES ──────────────────────────────────────────────────────────
categorize <- function(feature) {
  dplyr::case_when(
    feature %in% c("Team", "Opp")                                        ~ "Identity",
    feature == "second_half_game_num"                                     ~ "Game context",
    grepl("^opp_cum_win_rate|^opp_cum_run_diff|^opp_roll_win|^opp_slope_win|^opp_cum_ERA|^opp_cum_ER|^opp_cum_SO_pit|^opp_cum_BB_allowed|^opp_cum_H_allowed|^opp_cum_HR_allowed|^opp_cum_HBP|^opp_cum_WP|^opp_roll_ERA|^opp_roll_SO_pit|^opp_roll_BB_allowed|^opp_starter|^opp_bullpen", feature) ~ "Opponent pitching",
    grepl("^opp_cum_R|^opp_cum_H_per|^opp_cum_HR_per|^opp_cum_BB_per|^opp_cum_SO_per|^opp_cum_RBI|^opp_cum_SB|^opp_cum_LOB|^opp_cum_OBP|^opp_cum_SLG|^opp_cum_OPS|^opp_cum_BA|^opp_cum_XBH|^opp_cum_WPA|^opp_cum_RE24|^opp_roll_R|^opp_roll_OBP|^opp_roll_SLG|^opp_roll_OPS|^opp_roll_HR|^opp_roll_BB_per|^opp_roll_SO_per|^opp_slope_R|^opp_slope_OPS|^opp_q|^opp_iqr|^opp_cv", feature) ~ "Opponent batting",
    grepl("^opp_cum_RA|^opp_cum_run_diff|^opp_roll_RA|^opp_roll_run|^opp_slope_RA|^opp_slope_run", feature) ~ "Opponent outcome",
    grepl("^cum_ERA|^cum_ER|^cum_RA|^cum_H_allowed|^cum_HR_allowed|^cum_BB_allowed|^cum_SO_pit|^cum_HBP_pit|^cum_WP|^roll_ERA|^roll_RA|^roll_SO_pit|^roll_BB_allowed|^slope_ERA|^slope_RA|^starter|^bullpen", feature) ~ "Own pitching",
    grepl("^cum_R_per|^cum_H_per|^cum_HR_per|^cum_BB_per|^cum_SO_per|^cum_RBI|^cum_SB|^cum_LOB|^cum_OBP|^cum_SLG|^cum_OPS|^cum_BA|^cum_XBH|^cum_WPA|^cum_RE24|^roll_R_per|^roll_OBP|^roll_SLG|^roll_OPS|^roll_HR|^roll_BB_per|^roll_SO_per|^slope_R|^slope_OPS", feature) ~ "Own batting",
    grepl("^cum_win_rate|^cum_run_diff|^roll_win|^roll_run_diff|^slope_win|^slope_run_diff|^cv_run|^q25|^q75|^iqr", feature) ~ "Own outcome",
    TRUE ~ "Other"
  )
}

df <- df %>%
  mutate(category = categorize(feature))

# ── THEME ─────────────────────────────────────────────────────────────────────
cat_colors <- c(
  "Own batting"       = "#1D9E75",
  "Own pitching"      = "#378ADD",
  "Own outcome"       = "#639922",
  "Opponent batting"  = "#D4537E",
  "Opponent pitching" = "#D85A30",
  "Opponent outcome"  = "#BA7517",
  "Game context"      = "#888780",
  "Identity"          = "#B4B2A9"
)

col_grid <- "#EEEEEE"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(color = "#222222", size = 9,
                                      family = "mono"),
    axis.text.x        = element_text(color = "#666666", size = 8.5),
    axis.title.x       = element_text(color = "#444444", size = 9.5,
                                      margin = margin(t = 5)),
    axis.title.y       = element_blank(),
    plot.title         = element_text(color = "#111111", size = 12,
                                      face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(color = "#666666", size = 9.5,
                                      hjust = 0.5, margin = margin(b = 8)),
    legend.position    = "none",
    plot.margin        = margin(t = 8, r = 16, b = 8, l = 4)
  )

make_shap_panel <- function(model_name, title, n = 15) {
  sub <- df %>%
    filter(model == model_name,
           !feature %in% c("Team", "Opp")) %>%
    arrange(desc(mean_abs_shap)) %>%
    head(n) %>%
    arrange(mean_abs_shap) %>%
    mutate(feature = factor(feature, levels = feature))

  ggplot(sub, aes(x = mean_abs_shap, y = feature, fill = category)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.4f", mean_abs_shap)),
              hjust = -0.15, size = 2.7, color = "#333333") +
    scale_fill_manual(values = cat_colors) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                       labels = function(x) sprintf("%.3f", x)) +
    labs(title    = title,
         subtitle = sprintf("Top %d features by mean |SHAP| on validation set", n),
         x        = "Mean absolute SHAP value") +
    base_theme
}

# ── BUILD PANELS ──────────────────────────────────────────────────────────────
p_rs <- make_shap_panel("Runs Scored", "Runs scored model")
p_ra <- make_shap_panel("Runs Allowed", "Runs allowed model")
p_wl <- make_shap_panel("Win/Loss",    "Win/loss model")

# ── SHARED LEGEND ─────────────────────────────────────────────────────────────
legend_df <- data.frame(
  category = names(cat_colors),
  x = 1, y = seq_along(cat_colors)
)

p_legend <- ggplot(legend_df, aes(x = x, y = reorder(category, y),
                                   fill = category)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = cat_colors) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.text.y     = element_text(size = 9.5, color = "#333333",
                                   hjust = 0, margin = margin(l = 4)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 20, r = 4, b = 20, l = 4)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 0.8))

# ── COMBINE ───────────────────────────────────────────────────────────────────
final <- (p_rs | p_ra | p_wl | p_legend) +
  plot_layout(widths = c(1, 1, 1, 0.4)) +
  plot_annotation(
    title   = "SHAP Feature Importance — CatBoost Trade Deadline Impact Model",
    subtitle = paste0(
      "Mean absolute SHAP values computed on 2022-2023 validation set.\n",
      "Team/Opp categorical features excluded for readability. ",
      "Features colored by category."
    ),
    caption = paste0(
      "SHAP (SHapley Additive exPlanations) measures each feature's average contribution ",
      "to model predictions across all validation observations.\n",
      "Higher values indicate greater predictive importance. ",
      "Features selected via SHAP-guided subset optimization."
    )
  )

# ── SAVE ──────────────────────────────────────────────────────────────────────
ggsave("../visuals/shap_importance.png", final,
       width = 18, height = 8, dpi = 200, bg = "white")
cat("Saved: shap_importance.png\n")
