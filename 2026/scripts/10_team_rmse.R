library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

#  VISUAL 4 — TEAM-LEVEL RMSE COMPARISON
#  For each of 30 teams: model RMSE vs Pythagorean baseline RMSE.
#  Ordered by model improvement. RS and RA side by side.
#  t-test and McNemar results annotated at bottom.

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
  select(-.row)

# Overall stats for annotation
rmse_fn <- function(a, p) sqrt(mean((a - p)^2, na.rm = TRUE))
t_rs <- t.test((df$runs_scored - df$pred_runs_scored)^2,
               (df$runs_scored - df$cum_R_per_game)^2,
               paired = TRUE, alternative = "less")
t_ra <- t.test((df$runs_allowed - df$pred_runs_allowed)^2,
               (df$runs_allowed - df$cum_RA_per_game)^2,
               paired = TRUE, alternative = "less")
model_correct <- as.integer((df$pred_win_prob >= 0.5) == df$win)
pyth_correct  <- as.integer((df$cum_R_per_game > df$cum_RA_per_game) == df$win)
b <- sum(model_correct == 0 & pyth_correct == 1)
c <- sum(model_correct == 1 & pyth_correct == 0)
mn_stat <- (abs(b - c) - 1)^2 / (b + c)
mn_p    <- pchisq(mn_stat, df = 1, lower.tail = FALSE)

# Team-level RMSEs
team_stats <- df %>%
  group_by(Team) %>%
  summarise(
    rmse_rs_model = rmse_fn(runs_scored,  pred_runs_scored),
    rmse_rs_pyth  = rmse_fn(runs_scored,  cum_R_per_game),
    rmse_ra_model = rmse_fn(runs_allowed, pred_runs_allowed),
    rmse_ra_pyth  = rmse_fn(runs_allowed, cum_RA_per_game),
    acc_model     = mean(as.integer(pred_win_prob >= 0.5) == win),
    acc_pyth      = mean(as.integer(cum_R_per_game > cum_RA_per_game) == win),
    .groups = "drop"
  ) %>%
  mutate(
    rs_improvement = rmse_rs_pyth  - rmse_rs_model,
    ra_improvement = rmse_ra_pyth  - rmse_ra_model,
    wl_improvement = acc_model - acc_pyth,
    rs_color       = ifelse(rs_improvement >= 0, "Model better", "Baseline better"),
    ra_color       = ifelse(ra_improvement >= 0, "Model better", "Baseline better")
  )

# Order by RS improvement
team_order_rs <- team_stats %>% arrange(rs_improvement) %>% pull(Team)
team_order_ra <- team_stats %>% arrange(ra_improvement) %>% pull(Team)

col_model    <- "#1D9E75"
col_baseline <- "#D85A30"
col_grid     <- "#EEEEEE"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(color = "#222222", size = 9.5),
    axis.text.x        = element_text(color = "#666666", size = 9),
    axis.title.x       = element_text(color = "#444444", size = 10, margin = margin(t=5)),
    axis.title.y       = element_blank(),
    plot.title         = element_text(color = "#111111", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(color = "#666666", size = 9.5, hjust = 0.5, margin = margin(b=6)),
    legend.position    = "top",
    legend.text        = element_text(size = 10),
    plot.margin        = margin(10, 14, 8, 10)
  )

make_improvement_panel <- function(data, team_order, imp_col, color_col,
                                    title, subtitle, xlab) {
  data <- data %>%
    mutate(Team = factor(Team, levels = team_order))

  lim <- max(abs(data[[imp_col]]), na.rm = TRUE) * 1.15

  ggplot(data, aes_string(x = imp_col, y = "Team", fill = color_col)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, color = "#555555", linewidth = 0.6) +
    geom_text(aes_string(
                label = sprintf("sprintf('%%+.4f', %s)", imp_col),
                hjust = sprintf("ifelse(%s >= 0, -0.15, 1.15)", imp_col)),
              size = 2.7, color = "#222222") +
    scale_fill_manual(
      values = c("Model better" = col_model, "Baseline better" = col_baseline),
      name   = NULL
    ) +
    scale_x_continuous(limits = c(-lim, lim),
                       expand = expansion(mult = c(0.02, 0.02)),
                       labels = function(x) sprintf("%+.3f", x)) +
    labs(title = title, subtitle = subtitle, x = xlab) +
    base_theme
}

p_rs <- make_improvement_panel(
  team_stats, team_order_rs, "rs_improvement", "rs_color",
  "Runs Scored RMSE — Model vs Baseline",
  "RMSE improvement per team (baseline RMSE \u2212 model RMSE)\nGreen = model lower RMSE, orange = baseline lower",
  "RMSE improvement (runs) — positive = model better"
)

p_ra <- make_improvement_panel(
  team_stats, team_order_ra, "ra_improvement", "ra_color",
  "Runs Allowed RMSE — Model vs Baseline",
  "RMSE improvement per team (baseline RMSE \u2212 model RMSE)\nGreen = model lower RMSE, orange = baseline lower",
  "RMSE improvement (runs) — positive = model better"
)

# Summary stats annotation
n_rs_better <- sum(team_stats$rs_improvement > 0)
n_ra_better <- sum(team_stats$ra_improvement > 0)
n_wl_better <- sum(team_stats$wl_improvement > 0)

stats_label <- sprintf(
  paste0(
    "Overall model vs Pythagorean baseline (n=%d games)\n\n",
    "RS RMSE:  Model %.4f  vs  Baseline %.4f  (p=%.4f *)\n",
    "RA RMSE:  Model %.4f  vs  Baseline %.4f  (p=%.4f **)\n",
    "W/L Acc:  Model %.2f%%  vs  Baseline %.2f%%  (McNemar p=%.4f **)\n\n",
    "Model beats baseline for RS on %d/30 teams\n",
    "Model beats baseline for RA on %d/30 teams\n",
    "Model beats baseline for W/L on %d/30 teams"
  ),
  nrow(df),
  rmse_fn(df$runs_scored,  df$pred_runs_scored),
  rmse_fn(df$runs_scored,  df$cum_R_per_game),  t_rs$p.value,
  rmse_fn(df$runs_allowed, df$pred_runs_allowed),
  rmse_fn(df$runs_allowed, df$cum_RA_per_game), t_ra$p.value,
  mean(model_correct)*100, mean(pyth_correct)*100, mn_p,
  n_rs_better, n_ra_better, n_wl_better
)

p_stats <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = stats_label,
           hjust = 0.5, vjust = 0.5, size = 3.8, color = "#222222",
           lineheight = 1.65, fontface = "plain") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F4F9F4", color = "#1D9E75", linewidth = 1.2),
    plot.margin     = margin(14, 14, 14, 14)
  ) +
  xlim(0, 1) + ylim(0, 1)

final <- ((p_rs | p_ra) / p_stats) +
  plot_layout(heights = c(5, 1)) +
  plot_annotation(
    title    = "CatBoost Model vs Pythagorean Baseline — Team-Level RMSE Improvement",
    subtitle = "2025 Held-Out Test Set  |  Positive bars = model outperformed baseline for that team  |  * p\u22640.05  ** p\u22640.01",
    caption  = paste0(
      "RMSE improvement = baseline RMSE \u2212 model RMSE per team.\n",
      "Positive value means the model had lower prediction error than the naive Pythagorean baseline for that franchise.\n",
      "One-sided paired t-test tests H\u2081: model MSE < baseline MSE across all games."
    )
  )

ggsave("../model-validation/team_rmse.png", final,
       width = 16, height = 13, dpi = 200, bg = "white")

