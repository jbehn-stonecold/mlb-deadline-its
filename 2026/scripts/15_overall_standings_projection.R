library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)

#  OVERALL 2026 STANDINGS + PLAYOFF PICTURE PROJECTION
#
#  One consolidated graphic: every team's projected full-season record,
#  pre-deadline (actual) win% vs. post-deadline (projected) win%, and
#  playoff status (Division Winner / Wild Card / Missed Playoffs), grouped
#  by league and division.
#
#  Input:  Outputs\schedule_predictions_2026_team_summary.csv
#  Output: 2026 Post Deadline Predictions\2026_overall_standings_projection.png

OUT_DIR   <- "../outputs/"
PANEL_DIR <- "../predictions/"

# Real MLB division/league alignment (same as 13_playoff_projections)
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
  select(Team, proj_full_season_wins, proj_full_season_losses, proj_full_season_win_pct,
         pre_win_pct, proj_post_win_pct) %>%
  inner_join(divisions, by = "Team") %>%
  group_by(League, Division) %>%
  mutate(division_rank = rank(-proj_full_season_wins, ties.method = "first")) %>%
  ungroup()

wc_pool <- standings %>%
  filter(division_rank != 1) %>%
  group_by(League) %>%
  mutate(wc_rank = rank(-proj_full_season_wins, ties.method = "first")) %>%
  ungroup()

standings <- standings %>%
  left_join(wc_pool %>% select(Team, wc_rank), by = "Team") %>%
  mutate(
    playoff_status = case_when(
      division_rank == 1            ~ "Division Winner",
      !is.na(wc_rank) & wc_rank <= 3 ~ "Wild Card",
      TRUE                            ~ "Missed Playoffs"
    ),
    record     = sprintf("%.0f-%.0f", proj_full_season_wins, proj_full_season_losses),
    pre_pct    = sprintf("%.3f", pre_win_pct),
    post_pct   = sprintf("%.3f", proj_post_win_pct)
  )

col_winner  <- "#1D9E75"
col_wc      <- "#378ADD"
col_missed  <- "#B4B2A9"
col_header  <- "#222222"
col_text    <- "#222222"
col_grid    <- "#DDDDDD"

status_color <- function(status) case_when(
  status == "Division Winner" ~ col_winner,
  status == "Wild Card"       ~ col_wc,
  TRUE                        ~ col_missed
)

# Column x-positions for the manual "table" layout
col_x <- c(team = 0, record = 1.5, pre = 2.9, post = 4.3, status = 5.7)
x_max <- 7.2

make_division_table <- function(div_data, title_str) {
  div_data <- div_data %>%
    arrange(desc(proj_full_season_wins)) %>%
    mutate(
      row_y      = rev(seq_len(n())),
      status_col = status_color(playoff_status)
    )

  n_rows  <- nrow(div_data)
  y_top   <- n_rows + 1

  p <- ggplot(div_data) +
    # row background tint by playoff status
    geom_rect(aes(xmin = -0.2, xmax = x_max, ymin = row_y - 0.45, ymax = row_y + 0.45,
                  fill = status_col), alpha = 0.15) +
    scale_fill_identity() +
    # header row
    annotate("text", x = col_x, y = y_top, hjust = 0,
             label = c("Team", "Rec.", "Pre-Deadline", "Post-Deadline", "Status"),
             fontface = "bold", size = 3.1, color = col_header) +
    geom_hline(yintercept = y_top - 0.5, color = col_grid, linewidth = 0.5) +
    # data cells
    geom_text(aes(x = col_x["team"],   y = row_y, label = Team),
              hjust = 0, size = 3.2, fontface = "bold", color = col_text) +
    geom_text(aes(x = col_x["record"], y = row_y, label = record),
              hjust = 0, size = 3.0, color = col_text) +
    geom_text(aes(x = col_x["pre"],    y = row_y, label = pre_pct),
              hjust = 0, size = 3.0, color = col_text) +
    geom_text(aes(x = col_x["post"],   y = row_y, label = post_pct),
              hjust = 0, size = 3.0, color = col_text) +
    geom_text(aes(x = col_x["status"], y = row_y, label = playoff_status, color = status_col),
              hjust = 0, size = 2.8, fontface = "bold") +
    scale_color_identity() +
    scale_x_continuous(limits = c(-0.2, x_max), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, y_top + 0.6), expand = c(0, 0)) +
    labs(title = title_str) +
    theme_void(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 11.5, color = "#111111",
                                      margin = margin(b = 4)),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.margin      = margin(6, 10, 6, 10)
    )
  p
}

TEAM_LEAGUE <- function(lg, div) {
  standings %>% filter(League == lg, Division == div)
}

p_al_e <- make_division_table(TEAM_LEAGUE("AL", "East"),    "AL East")
p_al_c <- make_division_table(TEAM_LEAGUE("AL", "Central"), "AL Central")
p_al_w <- make_division_table(TEAM_LEAGUE("AL", "West"),    "AL West")
p_nl_e <- make_division_table(TEAM_LEAGUE("NL", "East"),    "NL East")
p_nl_c <- make_division_table(TEAM_LEAGUE("NL", "Central"), "NL Central")
p_nl_w <- make_division_table(TEAM_LEAGUE("NL", "West"),    "NL West")

p_al <- p_al_e / p_al_c / p_al_w
p_nl <- p_nl_e / p_nl_c / p_nl_w

# Legend panel
legend_text <- paste0(
  "Rec. = projected full-season W-L (actual first half + modeled rest of season)\n",
  "Pre-Deadline = actual first-half win%\n",
  "Post-Deadline = model-projected win% for the rest of the season\n\n",
  "Status colors:\n",
  "  Division Winner (green) — best record in division\n",
  "  Wild Card (blue) — top 3 non-division-winners per league\n",
  "  Missed Playoffs (gray) — outside the top 6 in league"
)

p_legend <- ggplot() +
  annotate("text", x = 0.02, y = 0.5, label = legend_text, hjust = 0, vjust = 0.5,
           size = 3.0, color = "#333333", lineheight = 1.4) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F8F8F8", color = "#DDDDDD", linewidth = 0.5),
        plot.margin = margin(8, 8, 8, 8)) +
  xlim(0, 1) + ylim(0, 1)

final <- (p_al | p_nl) / p_legend +
  plot_layout(heights = c(10, 2)) +
  plot_annotation(
    title    = "2026 MLB Projected Standings & Playoff Picture (Post-Trade-Deadline)",
    subtitle = "Based on actual first-half results plus the trained CatBoost model's projections for the remaining schedule",
    caption  = "Single-point projection, not a Monte Carlo simulation. Format: 3 division winners + 3 wild cards per league."
  )

out_file <- paste0(PANEL_DIR, "2026_overall_standings_projection.png")
ggsave(out_file, final, width = 16, height = 16.5, dpi = 200, bg = "white")
cat(sprintf("Saved %s\n", out_file))
