library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)

#  PLAYOFF PROJECTIONS — ONE PER TEAM, 2026
#
#  Projects each team's full-season standing (actual first half + modeled
#  rest-of-season from 11_build_and_predict_2026_schedule) against real MLB
#  divisions, using the current playoff format: 3 division winners + 3 wild
#  cards per league (6 per league, 12 total).
#
#  Input:  Outputs\schedule_predictions_2026_team_summary.csv
#  Output: one PNG per team in
#          2026 Post Deadline Predictions\Playoff Projections\<TEAM>_2026_playoff_projection.png

OUT_DIR   <- "../outputs/"
PANEL_DIR <- "../predictions/playoff-projections/"

# Real MLB division/league alignment
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
  inner_join(divisions, by = "Team")

missing_teams <- setdiff(divisions$Team, standings$Team)
if (length(missing_teams) > 0) {
  cat("WARNING: no projection found for:", paste(missing_teams, collapse = ", "), "\n")
  cat("(need schedule_predictions_2026_team_summary.csv to include all 30 teams)\n")
}

# Games-back helper: standard GB formula
gb <- function(w1, l1, w2, l2) ((w1 - w2) + (l2 - l1)) / 2

standings <- standings %>%
  group_by(League, Division) %>%
  mutate(division_rank = rank(-proj_full_season_wins, ties.method = "first")) %>%
  ungroup()

division_leaders <- standings %>% filter(division_rank == 1) %>%
  select(League, Division, leader_wins = proj_full_season_wins, leader_losses = proj_full_season_losses)

standings <- standings %>%
  left_join(division_leaders, by = c("League", "Division")) %>%
  mutate(gb_division = round(gb(leader_wins, leader_losses, proj_full_season_wins, proj_full_season_losses), 1))

wc_pool <- standings %>%
  filter(division_rank != 1) %>%
  group_by(League) %>%
  mutate(wc_rank = rank(-proj_full_season_wins, ties.method = "first")) %>%
  ungroup()

wc_cutoff <- wc_pool %>% filter(wc_rank == 3) %>%
  select(League, cutoff_wins = proj_full_season_wins, cutoff_losses = proj_full_season_losses)

wc_pool <- wc_pool %>%
  left_join(wc_cutoff, by = "League") %>%
  mutate(gb_wildcard = round(gb(cutoff_wins, cutoff_losses, proj_full_season_wins, proj_full_season_losses), 1))

standings <- standings %>%
  left_join(wc_pool %>% select(Team, wc_rank, gb_wildcard), by = "Team") %>%
  mutate(
    playoff_status = case_when(
      division_rank == 1            ~ "Division Winner",
      !is.na(wc_rank) & wc_rank <= 3 ~ "Wild Card",
      TRUE                            ~ "Missed Playoffs"
    )
  )

col_in    <- "#1D9E75"
col_out   <- "#B4B2A9"
col_focus <- "#378ADD"
col_grid  <- "#EEEEEE"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(color = "#222222", size = 10),
    axis.text.x        = element_text(color = "#666666", size = 9),
    axis.title.x       = element_text(color = "#444444", size = 10, margin = margin(t = 6)),
    axis.title.y       = element_blank(),
    plot.title         = element_text(color = "#111111", size = 12, face = "bold", hjust = 0),
    plot.subtitle      = element_text(color = "#666666", size = 9, hjust = 0, margin = margin(b = 4)),
    legend.position    = "none",
    plot.margin        = margin(t = 8, r = 14, b = 8, l = 10)
  )

TEAMS <- sort(unique(standings$Team))
cat(sprintf("Building playoff projection panels for %d teams\n", length(TEAMS)))

for (TEAM_NAME in TEAMS) {

  team_row <- standings %>% filter(Team == TEAM_NAME)
  if (nrow(team_row) == 0) next
  team_div    <- team_row$Division[1]
  team_league <- team_row$League[1]

  div_data <- standings %>%
    filter(Division == team_div) %>%
    arrange(desc(proj_full_season_wins)) %>%
    mutate(
      Team_f = factor(Team, levels = rev(Team)),
      highlight = case_when(Team == TEAM_NAME ~ "focus",
                            division_rank == 1 ~ "leader",
                            TRUE ~ "other"),
      label = sprintf("%.0f-%.0f", proj_full_season_wins, proj_full_season_losses)
    )

  p_div <- ggplot(div_data, aes(x = proj_full_season_wins, y = Team_f, fill = highlight)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = label), hjust = -0.1, size = 3, color = "#333333") +
    scale_fill_manual(values = c("focus" = col_focus, "leader" = col_in, "other" = col_out)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = sprintf("AL/NL %s Division — Projected Standings", team_div),
         subtitle = "Projected full-season W-L (actual first half + modeled rest of season)",
         x = "Projected wins") +
    base_theme

  wc_data <- wc_pool %>%
    filter(League == team_league) %>%
    arrange(desc(proj_full_season_wins)) %>%
    mutate(
      Team_f = factor(Team, levels = rev(Team)),
      highlight = case_when(Team == TEAM_NAME ~ "focus",
                            wc_rank <= 3 ~ "leader",
                            TRUE ~ "other"),
      label = sprintf("%.0f-%.0f", proj_full_season_wins, proj_full_season_losses)
    )

  p_wc <- ggplot(wc_data, aes(x = proj_full_season_wins, y = Team_f, fill = highlight)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = label), hjust = -0.1, size = 3, color = "#333333") +
    { if (sum(wc_data$wc_rank <= 3) > 0 & sum(wc_data$wc_rank > 3) > 0)
        geom_hline(yintercept = nrow(wc_data) - 3 + 0.5, color = col_focus, linewidth = 0.7, linetype = "dashed")
      else NULL } +
    scale_fill_manual(values = c("focus" = col_focus, "leader" = col_in, "other" = col_out)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = sprintf("%s Wild Card Race", team_league),
         subtitle = "Non-division-winners only  |  Top 3 (above dashed line) make the playoffs",
         x = "Projected wins") +
    base_theme

  status      <- team_row$playoff_status[1]
  status_line <- switch(status,
    "Division Winner" = sprintf("Projected to WIN the %s %s division.", team_league, team_div),
    "Wild Card"       = sprintf("Projected to claim a %s Wild Card spot (seed #%d).", team_league, team_row$wc_rank[1]),
    "Missed Playoffs" = sprintf("Projected to miss the playoffs — %.1f games back of the final %s Wild Card spot.",
                                 team_row$gb_wildcard[1], team_league)
  )
  gb_div_line <- if (team_row$division_rank[1] > 1) {
    sprintf("%.1f games back of the %s division lead.", team_row$gb_division[1], team_div)
  } else {
    "Leads the division."
  }

  summary_text <- paste0(
    TEAM_NAME, " — 2026 Playoff Projection\n\n",
    sprintf("Projected record: %.0f-%.0f (%.3f)\n",
            team_row$proj_full_season_wins[1], team_row$proj_full_season_losses[1],
            team_row$proj_full_season_win_pct[1]),
    sprintf("Division rank: #%d of 5 in %s %s\n", team_row$division_rank[1], team_league, team_div),
    gb_div_line, "\n\n",
    "Status: ", status, "\n",
    status_line
  )

  p_summary <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = summary_text, hjust = 0.5, vjust = 0.5,
             size = 3.6, color = "#222222", lineheight = 1.6) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#F4F9F4", color = col_in, linewidth = 1),
          plot.margin = margin(12, 12, 12, 12)) +
    xlim(0, 1) + ylim(0, 1)

  final <- (p_div | p_wc | p_summary) +
    plot_layout(widths = c(1, 1.1, 0.9)) +
    plot_annotation(
      title    = sprintf("%s — 2026 Playoff Projection", TEAM_NAME),
      subtitle = "Based on actual first-half results + trained model projections for the remaining schedule",
      caption  = "Format: 3 division winners + 3 wild cards per league (current MLB playoff structure). Not a Monte Carlo simulation — single-point projection from modeled win probabilities."
    )

  out_file <- sprintf("%s%s_2026_playoff_projection.png", PANEL_DIR, TEAM_NAME)
  ggsave(out_file, final, width = 16, height = 8, dpi = 200, bg = "white")
  cat(sprintf("  Saved %s (%s)\n", out_file, status))
}

cat("\nDone — playoff projection panels written to Playoff Projections\\\n")
