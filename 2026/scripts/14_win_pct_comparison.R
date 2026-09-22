library(dplyr)
library(ggplot2)

#  PRE- VS. PROJECTED-POST-DEADLINE WIN PERCENTAGE — ONE PER TEAM, 2026
#
#  Compares each team's actual first-half (pre-deadline) win percentage
#  against the model's projected win percentage for the rest of the season
#  (post-deadline). Since 2026 second-half games haven't been played, the
#  "post" bar is the model's projected rate, not an actual result.
#
#  Input:  Outputs\schedule_predictions_2026_team_summary.csv
#  Output: one PNG per team in
#          2026 Post Deadline Predictions\Win Percentage Comparison\<TEAM>_2026_win_pct_comparison.png

OUT_DIR   <- "../outputs/"
PANEL_DIR <- "../predictions/win-percentage-comparison/"

team_summary <- read.csv(paste0(OUT_DIR, "schedule_predictions_2026_team_summary.csv"),
                         stringsAsFactors = FALSE)

col_pre  <- "#378ADD"
col_up   <- "#1D9E75"
col_down <- "#D85A30"
col_grid <- "#EEEEEE"

base_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(color = "#222222", size = 11),
    axis.text.x        = element_text(color = "#666666", size = 10),
    axis.title.x       = element_text(color = "#444444", size = 10, margin = margin(t = 6)),
    axis.title.y       = element_blank(),
    plot.title         = element_text(color = "#111111", size = 13, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(color = "#666666", size = 10, hjust = 0.5, margin = margin(b = 8)),
    legend.position    = "none",
    plot.margin        = margin(t = 12, r = 20, b = 10, l = 14)
  )

TEAMS <- sort(unique(team_summary$Team))
cat(sprintf("Building win%% comparison panels for %d teams\n", length(TEAMS)))

for (TEAM_NAME in TEAMS) {

  row <- team_summary %>% filter(Team == TEAM_NAME)
  if (nrow(row) == 0) next

  delta      <- row$win_pct_delta[1]
  post_color <- if (delta >= 0) col_up else col_down
  delta_label <- sprintf("%+.3f", delta)

  plot_df <- tibble::tibble(
    period = factor(c("Pre-Deadline\n(First Half, Actual)", "Post-Deadline\n(Rest of Season, Projected)"),
                    levels = c("Pre-Deadline\n(First Half, Actual)", "Post-Deadline\n(Rest of Season, Projected)")),
    win_pct = c(row$pre_win_pct[1], row$proj_post_win_pct[1]),
    fill_color = c("pre", "post")
  )

  p <- ggplot(plot_df, aes(x = period, y = win_pct, fill = fill_color)) +
    geom_col(width = 0.55) +
    geom_hline(yintercept = 0.5, color = "#AAAAAA", linewidth = 0.5, linetype = "dashed") +
    geom_text(aes(label = sprintf("%.3f", win_pct)), vjust = -0.6, size = 4.5, color = "#222222", fontface = "bold") +
    scale_fill_manual(values = c("pre" = col_pre, "post" = post_color)) +
    scale_y_continuous(limits = c(0, max(0.75, plot_df$win_pct) * 1.15),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = sprintf("%s — Win Percentage: Pre- vs. Projected Post-Deadline", TEAM_NAME),
      subtitle = sprintf("Change: %s  (%s)", delta_label,
                         ifelse(delta >= 0, "projected to play better after the deadline",
                                            "projected to play worse after the deadline")),
      y = "Win percentage"
    ) +
    base_theme

  out_file <- sprintf("%s%s_2026_win_pct_comparison.png", PANEL_DIR, TEAM_NAME)
  ggsave(out_file, p, width = 8, height = 7, dpi = 200, bg = "white")
  cat(sprintf("  Saved %s (delta %s)\n", out_file, delta_label))
}

cat("\nDone — win percentage comparison panels written to Win Percentage Comparison\\\n")
