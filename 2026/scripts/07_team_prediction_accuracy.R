library(dplyr)
library(ggplot2)

#  TEAM PREDICTION ACCURACY — PERCENTAGE OFF FINAL RECORD
#
#  For each team shows what percentage of games the win projection was off by.
#  e.g. CLE: |34 - 27| / 54 = 13.0% off
#
#  Bars colored by direction: teal = model underestimated (team exceeded
#  projection), coral = model overestimated (team fell short of projection),
#  gray = exactly correct.
#
#  Input:  predictions_2026_team_summary.csv
#  Output: team_prediction_accuracy.png

df <- read.csv("../outputs/predictions_2026_team_summary.csv", stringsAsFactors = FALSE)

df <- df %>%
  mutate(
    error_wins = abs(actual_wins - proj_wins),
    pct_off    = round(error_wins / games_played * 100, 1),
    direction  = case_when(
      impact_wins > 0 ~ "exceeded",
      impact_wins < 0 ~ "fell short",
      TRUE            ~ "exact"
    ),
    label = sprintf("%d/%d\n%.1f%%", error_wins, games_played, pct_off)
  ) %>%
  arrange(pct_off) %>%
  mutate(Team = factor(Team, levels = Team))

# Summary stats for subtitle
mean_pct  <- round(mean(df$pct_off), 1)
exact_n   <- sum(df$direction == "exact")

col_exceed <- "#1D9E75"
col_short  <- "#D85A30"
col_exact  <- "#B4B2A9"
col_grid   <- "#EEEEEE"
col_avg    <- "#378ADD"

p <- ggplot(df, aes(x = pct_off, y = Team, fill = direction)) +
  geom_col(width = 0.72) +

  # Average line
  geom_vline(xintercept = mean_pct, color = col_avg,
             linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = mean_pct + 0.3, y = nrow(df) + 0.8,
           label = sprintf("Avg: %.1f%%", mean_pct),
           color = col_avg, size = 3.2, hjust = 0, fontface = "bold") +

  # Value labels
  geom_text(aes(label = label), hjust = -0.08,
            size = 2.6, color = "#333333", lineheight = 0.9) +

  # Actual vs projected annotation inside bar for context
  geom_text(aes(label = sprintf("Proj %d  Act %d", proj_wins, actual_wins),
                x = 0.3),
            hjust = 0, size = 2.4, color = "white", fontface = "bold") +

  scale_fill_manual(
    values  = c("exceeded"  = col_exceed,
                "fell short" = col_short,
                "exact"     = col_exact),
    name    = NULL,
    labels  = c("exceeded"  = "Team exceeded projection",
                "fell short" = "Team fell short of projection",
                "exact"     = "Exact match")
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    limits = c(0, 18),
    breaks = seq(0, 16, by = 2),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "2026 Second Half — Team Win Prediction Accuracy",
    subtitle = sprintf(
      "How far off was the W/L model projection from each team's actual win total?\nOrdered by error size  |  Average error: %.1f%%  |  %d teams predicted exactly",
      mean_pct, exact_n),
    caption  = paste0(
      "Percentage calculated as |actual wins - projected wins| / games played.\n",
      "Projected wins = cumulative sum of per-game win probabilities from W/L model.\n",
      "Labels show absolute error (wins), games played, and percentage off."
    ),
    x = "Percentage of games projection was off",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(color = "#222222", size = 10.5, hjust = 1),
    axis.text.x        = element_text(color = "#666666", size = 9.5),
    axis.title.x       = element_text(color = "#444444", size = 10,
                                      margin = margin(t = 8)),
    plot.title         = element_text(color = "#111111", size = 14,
                                      face = "bold", hjust = 0),
    plot.subtitle      = element_text(color = "#666666", size = 9.5,
                                      hjust = 0, margin = margin(b = 10)),
    plot.caption       = element_text(color = "#999999", size = 8.5, hjust = 0),
    legend.position    = "top",
    legend.text        = element_text(size = 10, color = "#444444"),
    legend.key.size    = unit(0.8, "lines"),
    plot.margin        = margin(t = 12, r = 16, b = 10, l = 10)
  )

ggsave("../model-validation/team_prediction_accuracy.png", p,
       width = 12, height = 11, dpi = 200, bg = "white")
