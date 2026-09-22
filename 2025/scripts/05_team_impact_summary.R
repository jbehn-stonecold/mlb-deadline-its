library(dplyr)
library(ggplot2)
library(patchwork)

# ── LOAD ──────────────────────────────────────────────────────────────────────
df <- read.csv("../outputs/predictions_2025_team_summary.csv", stringsAsFactors = FALSE)

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
    # CI as formatted text labels
    win_ci_label = sprintf("(%d, %d)", impact_wins_ci_lo, impact_wins_ci_hi),
    rs_ci_label  = sprintf("(%.2f, %.2f)", impact_rs_ci_lo, impact_rs_ci_hi),
    ra_ci_label  = sprintf("(%.2f, %.2f)", impact_ra_ci_lo, impact_ra_ci_hi)
  )

col_above <- "#1D9E75"
col_below <- "#D85A30"
col_zero  <- "#B4B2A9"
col_grid  <- "#EEEEEE"
col_ci    <- "#888780"

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

# ── PANEL 1 — WINS ────────────────────────────────────────────────────────────
x_lim_w <- max(abs(df$impact_wins)) + 1

p_wins <- ggplot(df, aes(x = impact_wins, y = Team, fill = win_color)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
  # Impact value label — outside the bar
  geom_text(
    aes(label = ifelse(impact_wins >= 0, paste0("+", impact_wins), as.character(impact_wins)),
        hjust = ifelse(impact_wins >= 0, -0.2, 1.2)),
    size = 2.8, color = "#333333"
  ) +
  # CI label — on opposite side of bar from value
  geom_text(
    aes(label = win_ci_label,
        x     = ifelse(impact_wins >= 0, -0.15, 0.15),
        hjust = ifelse(impact_wins >= 0, 1, 0)),
    size = 2.3, color = col_ci, fontface = "italic"
  ) +
  scale_impact +
  scale_x_continuous(
    limits = c(-x_lim_w - 2, x_lim_w + 2),
    breaks = seq(-10, 10, by = 2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(title    = "Wins vs projection",
       subtitle = "95% CI shown as (lo, hi)",
       x        = "Wins above/below projected") +
  base_theme

# ── PANEL 2 — RUNS SCORED ─────────────────────────────────────────────────────
x_lim_rs <- max(abs(df$impact_rs_per_game)) + 0.15

p_rs <- ggplot(df, aes(x = impact_rs_per_game, y = Team, fill = rs_color)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+.2f", impact_rs_per_game),
        hjust = ifelse(impact_rs_per_game >= 0, -0.2, 1.2)),
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
    limits = c(-x_lim_rs - 0.3, x_lim_rs + 0.3),
    breaks = seq(-1.4, 1.4, by = 0.4),
    labels = function(x) sprintf("%+.1f", x),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(title    = "Runs scored/game vs projection",
       subtitle = "95% CI shown as (lo, hi)",
       x        = "R/G above/below projected") +
  base_theme +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# ── PANEL 3 — RUNS ALLOWED ────────────────────────────────────────────────────
x_lim_ra <- max(abs(df$impact_ra_per_game)) + 0.15

p_ra <- ggplot(df, aes(x = impact_ra_per_game, y = Team, fill = ra_color)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, color = "#AAAAAA", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%+.2f", impact_ra_per_game),
        hjust = ifelse(impact_ra_per_game >= 0, -0.2, 1.2)),
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
    limits = c(-x_lim_ra - 0.3, x_lim_ra + 0.3),
    breaks = seq(-1.4, 1.6, by = 0.4),
    labels = function(x) sprintf("%+.1f", x),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(title    = "Runs allowed/game vs projection",
       subtitle = "95% CI shown as (lo, hi)",
       x        = "RA/G above/below projected\n(negative = better pitching)") +
  base_theme +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# ── COMBINE ───────────────────────────────────────────────────────────────────
final <- (p_wins | p_rs | p_ra) +
  plot_layout(widths = c(1.2, 1, 1)) +
  plot_annotation(
    title    = "2025 MLB Trade Deadline Impact — Second Half vs Historical Projection",
    subtitle = "Green = outperformed projection  |  Red = underperformed  |  Ordered by wins above expectation",
    caption  = paste0(
      "95% confidence intervals shown in italics as (lower bound, upper bound) on the opposite side of each bar from the point estimate.\n",
      "Wins CI: Bernoulli variance of summed win probabilities. RS/G and RA/G CI: RMSE / sqrt(n games).\n",
      "Negative RA impact = team allowed fewer runs than projected (better pitching than expected)."
    )
  )

ggsave("../visuals/2025_deadline_impact_summary.png", final,
       width = 16, height = 10, dpi = 200, bg = "white")
cat("Saved: 2025_deadline_impact_summary.png\n")
