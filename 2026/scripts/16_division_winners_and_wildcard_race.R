library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)

#  DIVISION WINNERS + WILD CARD RACE (GAMES BACK)
#
#  Top: the 3 division winners per league, one per division.
#  Bottom: the wild card race per league — every non-division-winner,
#  ranked by projected full-season wins, with games back from the final
#  (3rd) wild card spot and which division each team belongs to.
#
#  Input:  Outputs\schedule_predictions_2026_team_summary.csv
#  Output: 2026 Post Deadline Predictions\2026_division_winners_and_wildcard_race.png

OUT_DIR   <- "../outputs/"
PANEL_DIR <- "../predictions/"

# Real MLB division/league alignment (same as 13/15)
divisions <- tribble(
  ~Team, ~League, ~Division,
  "NYY","AL","East",  "BOS","AL","East",  "TBR","AL","East",  "TOR","AL","East",  "BAL","AL","East",
  "CLE","AL","Central","MIN","AL","Central","DET","AL","Central","CHW","AL","Central","KCR","AL","Central",
  "HOU","AL","West",  "SEA","AL","West",  "TEX","AL","West",  "LAA","AL","West",  "OAK","AL","West",
  "ATL","NL","East",  "PHI","NL","East",  "NYM","NL","East",  "MIA","NL","East",  "WSN","NL","East",
  "MIL","NL","Central","CHC","NL","Central","STL","NL","Central","CIN","NL","Central","PIT","NL","Central",
  "LAD","NL","West",  "SDP","NL","West",  "ARI","NL","West",  "SFG","NL","West",  "COL","NL","West"
)

team_summary <- read.csv(paste0(OUT_DIR, "schedule_predictions_2026_team_summary.csv"),
                         stringsAsFactors = FALSE)

standings <- team_summary %>%
  select(Team, proj_full_season_wins, proj_full_season_losses, proj_full_season_win_pct) %>%
  inner_join(divisions, by = "Team") %>%
  group_by(League, Division) %>%
  mutate(division_rank = rank(-proj_full_season_wins, ties.method = "first")) %>%
  ungroup()

division_winners <- standings %>%
  filter(division_rank == 1) %>%
  mutate(record = sprintf("%.0f-%.0f", proj_full_season_wins, proj_full_season_losses),
         win_pct = sprintf("%.3f", proj_full_season_win_pct))

# Games-back helper: standard GB formula
gb_formula <- function(w1, l1, w2, l2) ((w1 - w2) + (l2 - l1)) / 2

division_leaders <- division_winners %>%
  select(League, Division, leader_wins = proj_full_season_wins, leader_losses = proj_full_season_losses)

wc_pool <- standings %>%
  filter(division_rank != 1) %>%
  left_join(division_leaders, by = c("League", "Division")) %>%
  group_by(League) %>%
  mutate(wc_rank = rank(-proj_full_season_wins, ties.method = "first")) %>%
  ungroup()

wc_cutoff <- wc_pool %>%
  filter(wc_rank == 3) %>%
  select(League, cutoff_wins = proj_full_season_wins, cutoff_losses = proj_full_season_losses)

wc_pool <- wc_pool %>%
  left_join(wc_cutoff, by = "League") %>%
  mutate(
    gb          = gb_formula(cutoff_wins, cutoff_losses, proj_full_season_wins, proj_full_season_losses),
    gb_label    = case_when(
      wc_rank == 3 ~ "—",
      wc_rank <  3 ~ sprintf("+%.1f", -gb),
      TRUE         ~ sprintf("%.1f", gb)
    ),
    div_gb      = gb_formula(leader_wins, leader_losses, proj_full_season_wins, proj_full_season_losses),
    div_gb_label = sprintf("%.1f", div_gb),
    record  = sprintf("%.0f-%.0f", proj_full_season_wins, proj_full_season_losses),
    win_pct = sprintf("%.3f", proj_full_season_win_pct),
    in_race = wc_rank <= 3
  )

col_winner <- "#1D9E75"
col_in     <- "#378ADD"
col_out    <- "#888880"
col_header <- "#222222"
col_text   <- "#222222"
col_grid   <- "#DDDDDD"

# ── DIVISION WINNERS PANEL (one per league) ───────────────────────────────────
make_winners_panel <- function(lg) {
  data <- division_winners %>%
    filter(League == lg) %>%
    mutate(Division = factor(Division, levels = c("East", "Central", "West"))) %>%
    arrange(Division) %>%
    mutate(row_y = rev(seq_len(n())))

  col_x <- c(division = 0, team = 1.6, record = 3.0, pct = 4.4)
  x_max <- 5.6
  y_top <- nrow(data) + 1

  ggplot(data) +
    geom_rect(aes(xmin = -0.2, xmax = x_max, ymin = row_y - 0.45, ymax = row_y + 0.45),
              fill = col_winner, alpha = 0.15) +
    annotate("text", x = col_x, y = y_top, hjust = 0,
             label = c("Division", "Team", "Rec.", "Win%"),
             fontface = "bold", size = 3.1, color = col_header) +
    geom_hline(yintercept = y_top - 0.5, color = col_grid, linewidth = 0.5) +
    geom_text(aes(x = col_x["division"], y = row_y, label = paste(League, Division)),
              hjust = 0, size = 3.0, color = col_text) +
    geom_text(aes(x = col_x["team"],   y = row_y, label = Team),
              hjust = 0, size = 3.2, fontface = "bold", color = col_winner) +
    geom_text(aes(x = col_x["record"], y = row_y, label = record),
              hjust = 0, size = 3.0, color = col_text) +
    geom_text(aes(x = col_x["pct"],    y = row_y, label = win_pct),
              hjust = 0, size = 3.0, color = col_text) +
    scale_x_continuous(limits = c(-0.2, x_max), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, y_top + 0.6), expand = c(0, 0)) +
    labs(title = sprintf("%s Division Winners", lg)) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 12, color = "#111111", margin = margin(b = 4)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(6, 10, 6, 10)
    )
}

# ── WILD CARD RACE PANEL (one per league) ─────────────────────────────────────
make_wc_panel <- function(lg) {
  data <- wc_pool %>%
    filter(League == lg) %>%
    arrange(wc_rank) %>%
    mutate(row_y = rev(seq_len(n())),
           row_color = ifelse(in_race, col_in, col_out))

  col_x <- c(division = 0, team = 1.4, record = 2.8, pct = 4.0, div_gb = 5.1, gb = 6.2)
  x_max <- 7.5
  n_rows <- nrow(data)
  y_top  <- n_rows + 1
  cutoff_y <- n_rows - 3 + 0.5

  ggplot(data) +
    geom_rect(aes(xmin = -0.2, xmax = x_max, ymin = row_y - 0.45, ymax = row_y + 0.45,
                  fill = row_color), alpha = 0.15) +
    scale_fill_identity() +
    annotate("text", x = col_x, y = y_top, hjust = 0,
             label = c("Division", "Team", "Rec.", "Win%", "Div. GB", "WC GB"),
             fontface = "bold", size = 3.1, color = col_header) +
    geom_hline(yintercept = y_top - 0.5, color = col_grid, linewidth = 0.5) +
    geom_hline(yintercept = cutoff_y, color = col_in, linewidth = 0.8, linetype = "dashed") +
    geom_text(aes(x = col_x["division"], y = row_y, label = paste(League, Division)),
              hjust = 0, size = 2.9, color = col_text) +
    geom_text(aes(x = col_x["team"],   y = row_y, label = Team, color = row_color),
              hjust = 0, size = 3.1, fontface = "bold") +
    geom_text(aes(x = col_x["record"], y = row_y, label = record),
              hjust = 0, size = 2.9, color = col_text) +
    geom_text(aes(x = col_x["pct"],    y = row_y, label = win_pct),
              hjust = 0, size = 2.9, color = col_text) +
    geom_text(aes(x = col_x["div_gb"], y = row_y, label = div_gb_label),
              hjust = 0, size = 2.9, color = col_text) +
    geom_text(aes(x = col_x["gb"],     y = row_y, label = gb_label, color = row_color),
              hjust = 0, size = 2.9, fontface = "bold") +
    scale_color_identity() +
    scale_x_continuous(limits = c(-0.2, x_max), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, y_top + 0.6), expand = c(0, 0)) +
    labs(title = sprintf("%s Wild Card Race", lg),
         subtitle = "Top 3 (above dashed line) hold a playoff spot") +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 12, color = "#111111", margin = margin(b = 2)),
      plot.subtitle   = element_text(size = 9, color = "#666666", margin = margin(b = 4)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(6, 10, 6, 10)
    )
}

p_al_winners <- make_winners_panel("AL")
p_nl_winners <- make_winners_panel("NL")
p_al_wc      <- make_wc_panel("AL")
p_nl_wc      <- make_wc_panel("NL")

legend_text <- paste0(
  "Rec. = projected full-season W-L (actual first half + modeled rest of season)\n",
  "Div. GB = games back of that team's own division leader\n",
  "WC GB = games back from the final (3rd) wild card spot in that league ",
  "(\"+X\" = games clear of missing it, \"\u2014\" = exactly on the cutoff)\n\n",
  "Green = division winner   |   Blue = holding a wild card spot   |   Gray = outside the playoff picture"
)

p_legend <- ggplot() +
  annotate("text", x = 0.02, y = 0.5, label = legend_text, hjust = 0, vjust = 0.5,
           size = 3.0, color = "#333333", lineheight = 1.4) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F8F8F8", color = "#DDDDDD", linewidth = 0.5),
        plot.margin = margin(8, 8, 8, 8)) +
  xlim(0, 1) + ylim(0, 1)

final <- (p_al_winners | p_nl_winners) / (p_al_wc | p_nl_wc) / p_legend +
  plot_layout(heights = c(3.2, 5.5, 1.6)) +
  plot_annotation(
    title    = "2026 Division Winners & Wild Card Race (Post-Trade-Deadline)",
    subtitle = "Based on actual first-half results plus the trained CatBoost model's projections for the remaining schedule",
    caption  = "Single-point projection, not a Monte Carlo simulation. Format: 3 division winners + 3 wild cards per league."
  )

out_file <- paste0(PANEL_DIR, "2026_division_winners_and_wildcard_race.png")
ggsave(out_file, final, width = 15, height = 15, dpi = 200, bg = "white")
cat(sprintf("Saved %s\n", out_file))
