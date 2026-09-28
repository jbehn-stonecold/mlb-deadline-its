library(dplyr)
library(ggplot2)
library(patchwork)

#  TEAM IMPACT SUMMARY — 90% CI LABELS AND FORECAST COVERAGE STARS
#
#  Stars = forecast coverage (how close actual was to projection):
#    *** actual within 90% CI — tightest band, best prediction
#    **  actual within 95% CI but outside 90% CI
#    *   actual within 99% CI but outside 95% CI
#    (blank) actual outside 99% CI — model significantly missed
#

df <- read.csv("../outputs/predictions_2026_team_summary.csv", stringsAsFactors = FALSE)

df <- df %>%
  arrange(desc(impact_wins)) %>%
  mutate(
    Team      = factor(Team, levels = rev(Team)),
    win_color = ifelse(impact_wins > 0, "above", ifelse(impact_wins < 0, "below", "zero")),
    rs_color  = ifelse(impact_rs_per_game > 0, "above", ifelse(impact_rs_per_game < 0, "below", "zero")),
    ra_color  = ifelse(impact_ra_per_game < 0, "above", ifelse(impact_ra_per_game > 0, "below", "zero")),

    # CI bounds on impact (actual is fixed, uncertainty is in projection)
    impact_wins_ci_lo = actual_wins - proj_wins_ci_hi,
    impact_wins_ci_hi = actual_wins - proj_wins_ci_lo,
    impact_rs_ci_lo   = round(actual_rs_per_game - proj_rs_pg_ci_hi, 2),
    impact_rs_ci_hi   = round(actual_rs_per_game - proj_rs_pg_ci_lo, 2),
    impact_ra_ci_lo   = round(actual_ra_per_game - proj_ra_pg_ci_hi, 2),
    impact_ra_ci_hi   = round(actual_ra_per_game - proj_ra_pg_ci_lo, 2),

    # CI text labels
    win_ci_label = sprintf("(%d, %d)", impact_wins_ci_lo, impact_wins_ci_hi),
    rs_ci_label  = sprintf("(%.2f, %.2f)", impact_rs_ci_lo, impact_rs_ci_hi),
    ra_ci_label  = sprintf("(%.2f, %.2f)", impact_ra_ci_lo, impact_ra_ci_hi),

    # Value + stars combined label
    win_label = paste0(ifelse(impact_wins >= 0, "+", ""), impact_wins,
                       ifelse(wins_stars != "", paste0(" ", wins_stars), "")),
    rs_label  = paste0(sprintf("%+.2f", impact_rs_per_game),
                       ifelse(rs_stars != "", paste0(" ", rs_stars), "")),
    ra_label  = paste0(sprintf("%+.2f", impact_ra_per_game),
                       ifelse(ra_stars != "", paste0(" ", ra_stars), ""))
  )

col_above <- "#1D9E75"
col_below <- "#D85A30"
col_zero  <- "#B4B2A9"
col_ci    <- "#888780"
col_grid  <- "#EEEEEE"

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
    axis.text.y        = element_text(color = "#222222", size = 10.5, hjust = 1),
    axis.text.x        = element_text(color = "#666666", size = 9),
    axis.title.x       = element_text(color = "#444444", size = 10, margin = margin(t = 6)),
    axis.title.y       = element_blank(),
    plot.title         = element_text(color = "#111111", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(color = "#666666", size = 9, hjust = 0.5,
                                      margin = margin(b = 4)),
    plot.margin        = margin(t = 8, r = 12, b = 8, l = 4)
  )

# PANEL 1 — WINS
x_lim_w <- max(abs(df$impact_wins)) + 1

p_wins <- ggplot(df, aes(x = impact_wins, y = Team, fill = win_color)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
  geom_text(
    aes(label = win_label,
        hjust = ifelse(impact_wins >= 0, -0.15, 1.15)),
    size = 2.8, color = "#333333"
  ) +
  geom_text(
    aes(label = win_ci_label,
        x     = ifelse(impact_wins >= 0, -0.15, 0.15),
        hjust = ifelse(impact_wins >= 0, 1, 0)),
    size = 2.2, color = col_ci, fontface = "italic"
  ) +
  scale_impact +
  scale_x_continuous(
    limits = c(-x_lim_w - 3, x_lim_w + 3),
    breaks = seq(-10, 10, by = 2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(title    = "Wins vs projection",
       subtitle = "90% CI as (lo, hi)  |  Stars = forecast coverage",
       x        = "Wins above/below projected") +
  base_theme

# PANEL 2 — RUNS SCORED
x_lim_rs <- max(abs(df$impact_rs_per_game)) + 0.15

p_rs <- ggplot(df, aes(x = impact_rs_per_game, y = Team, fill = rs_color)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
  geom_text(
    aes(label = rs_label,
        hjust = ifelse(impact_rs_per_game >= 0, -0.15, 1.15)),
    size = 2.8, color = "#333333"
  ) +
  geom_text(
    aes(label = rs_ci_label,
        x     = ifelse(impact_rs_per_game >= 0, -0.02, 0.02),
        hjust = ifelse(impact_rs_per_game >= 0, 1, 0)),
    size = 2.2, color = col_ci, fontface = "italic"
  ) +
  scale_impact +
  scale_x_continuous(
    limits = c(-x_lim_rs - 0.35, x_lim_rs + 0.35),
    breaks = seq(-1.4, 1.4, by = 0.4),
    labels = function(x) sprintf("%+.1f", x),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(title    = "Runs scored/game vs projection",
       subtitle = "90% CI as (lo, hi)  |  Stars = forecast coverage",
       x        = "R/G above/below projected") +
  base_theme +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# PANEL 3 — RUNS ALLOWED
x_lim_ra <- max(abs(df$impact_ra_per_game)) + 0.15

p_ra <- ggplot(df, aes(x = impact_ra_per_game, y = Team, fill = ra_color)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
  geom_text(
    aes(label = ra_label,
        hjust = ifelse(impact_ra_per_game >= 0, -0.15, 1.15)),
    size = 2.8, color = "#333333"
  ) +
  geom_text(
    aes(label = ra_ci_label,
        x     = ifelse(impact_ra_per_game >= 0, -0.02, 0.02),
        hjust = ifelse(impact_ra_per_game >= 0, 1, 0)),
    size = 2.2, color = col_ci, fontface = "italic"
  ) +
  scale_impact +
  scale_x_continuous(
    limits = c(-x_lim_ra - 0.35, x_lim_ra + 0.35),
    breaks = seq(-1.4, 1.6, by = 0.4),
    labels = function(x) sprintf("%+.1f", x),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(title    = "Runs allowed/game vs projection",
       subtitle = "90% CI as (lo, hi)  |  Stars = forecast coverage",
       x        = "RA/G above/below projected\n(negative = better pitching)") +
  base_theme +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# STAR LEGEND PANEL
legend_text <- paste0(
  "Forecast coverage stars\n\n",
  "*** Actual within 90% CI\n",
  "     Tightest band \u2014 best prediction\n\n",
  "**  Actual within 95% CI\n",
  "     (outside 90% CI)\n\n",
  "*   Actual within 99% CI\n",
  "     (outside 95% CI)\n\n",
  "(blank) Outside 99% CI\n",
  "     Model significantly\n",
  "     missed this team"
)

p_legend <- ggplot() +
  annotate("text", x = 0.05, y = 0.5,
           label = legend_text,
           hjust = 0, vjust = 0.5,
           size = 3.1, color = "#333333",
           lineheight = 1.5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F4F9F4", color = "#1D9E75",
                                   linewidth = 0.8),
    plot.margin = margin(8, 8, 8, 8)
  ) +
  xlim(0, 1) + ylim(0, 1)

# COMBINE
final <- (p_wins | p_rs | p_ra | p_legend) +
  plot_layout(widths = c(1.2, 1, 1, 0.55)) +
  plot_annotation(
    title    = "2026 MLB Trade Deadline Impact — Second Half vs Historical Projection",
    subtitle = "Green = outperformed projection  |  Red = underperformed  |  Ordered by wins above expectation",
    caption  = paste0(
      "Excess returns: difference between actual second-half performance and counterfactual projection from pre-deadline first-half profile.\n",
      "90% CI shown as (lo, hi) in italics on opposite side of each bar. ",
      "Negative RA impact = fewer runs allowed than projected (better pitching than expected)."
    )
  )

ggsave("../model-validation/2026_deadline_impact_summary.png", final,
       width = 18, height = 10, dpi = 200, bg = "white")
