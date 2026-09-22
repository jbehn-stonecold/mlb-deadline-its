library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)
library(readxl)

#  TEAM PROJECTION PANELS — ONE PER TEAM, 2026
#
#  Adapted from the old ITS panel (06_its_visualization) for a season that
#  hasn't finished yet: shows the real first-half 2026 trend plus the
#  model's projected second-half trend (dashed) — there is no "actual
#  second half" line because those games haven't been played. Includes the
#  same trade-moves panel per team.
#
#  Input:  data/stathead_team.csv, data/trades_2026.xlsx,
#          outputs/schedule_predictions_2026_game_level.csv
#  Output: one PNG per team in
#          predictions/team-projections/<TEAM>_2026_projection.png

DATA_DIR   <- "../data/"
OUT_DIR    <- "../outputs/"
PANEL_DIR  <- "../predictions/team-projections/"
# Manually-maintained trade-deadline tracker; not produced by the scrapers,
# supply your own copy at data/trades_2026.xlsx
TRADES_FILE <- "../data/trades_2026.xlsx"
SEASON      <- 2026
DEADLINE    <- as.Date(sprintf("%d-08-03", SEASON))  # 2026 trade deadline moved to Aug 3

# Mapping from Excel abbreviations to model abbreviations (same as 06)
abbrev_map <- c("TB" = "TBR", "KC" = "KCR", "SD" = "SDP", "SF" = "SFG",
                "ATH" = "OAK", "CWS" = "CHW", "WSH" = "WSN")
rev_map <- setNames(names(abbrev_map), abbrev_map)

build_moves <- function(trades_df) {
  if (nrow(trades_df) == 0) return(c("No recorded trades at the 2026 deadline."))
  lines <- character(0)
  for (i in seq_len(nrow(trades_df))) {
    row <- trades_df[i, ]
    added   <- trimws(row$Players_added)
    added_p <- trimws(row$pA_pos)
    given   <- trimws(row$Players_given)
    given_p <- trimws(row$pG_pos)
    acq_str <- if (!is.na(added) && tolower(added) != "cash") sprintf("Acquired: %s (%s)", added, added_p) else NULL
    trd_str <- if (!is.na(given) && tolower(given) != "cash") sprintf("Traded: %s (%s)", given, given_p) else NULL
    trade_line <- paste(c(acq_str, trd_str), collapse = " | ")
    if (nchar(trade_line) > 0) lines <- c(lines, trade_line)
  }
  lines
}

trades_raw <- tryCatch({
  t <- read_excel(TRADES_FILE, sheet = "Sheet1")
  colnames(t) <- c("Team", "Players_added", "pA_pos", "Players_given", "pG_pos",
                    "Trade_count", "date_of_trade", "notes")
  t
}, error = function(e) {
  cat("Could not read trades file (", TRADES_FILE, ") — moves panel will show 'no data'.\n")
  NULL
})

# ── LOAD DATA ──────────────────────────────────────────────────────────────────
raw <- read.csv(paste0(DATA_DIR, "stathead_team.csv"), stringsAsFactors = FALSE)
raw <- raw %>%
  mutate(
    Team   = dplyr::recode(Team, "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK"),
    Date   = as.Date(Date),
    Season = scrape_year
  )
score_match <- regmatches(raw$Result, regexpr("[0-9]+-[0-9]+", raw$Result))
score_split <- strsplit(score_match, "-")
raw$runs_scored  <- as.integer(sapply(score_split, `[`, 1))
raw$runs_allowed <- as.integer(sapply(score_split, `[`, 2))
raw$win          <- as.integer(substr(raw$Result, 1, 1) == "W")

schedule_preds <- read.csv(paste0(OUT_DIR, "schedule_predictions_2026_game_level.csv"),
                           stringsAsFactors = FALSE)
schedule_preds$Date <- as.Date(schedule_preds$Date)

TEAMS <- sort(unique(raw$Team[raw$Season == SEASON]))
cat(sprintf("Building team projection panels for %d teams\n", length(TEAMS)))

col_actual   <- "#1D9E75"
col_proj     <- "#378ADD"
col_deadline <- "#D85A30"
col_grid     <- "#EEEEEE"

its_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#555555", size = 10),
    axis.title       = element_text(color = "#333333", size = 11),
    plot.title       = element_text(color = "#111111", size = 13, face = "bold", hjust = 0),
    plot.subtitle    = element_text(color = "#666666", size = 10, hjust = 0, margin = margin(b = 6)),
    legend.position  = "top",
    legend.text      = element_text(color = "#444444", size = 10),
    legend.key.size  = unit(0.8, "lines"),
    plot.margin      = margin(t = 10, r = 20, b = 6, l = 10)
  )

for (TEAM_NAME in TEAMS) {

  first_half <- raw %>%
    filter(Team == TEAM_NAME, Season == SEASON, Date <= DEADLINE) %>%
    arrange(Date) %>%
    mutate(
      game_num = row_number(),
      cum_wins = cumsum(win),
      rs_roll  = rollmean(runs_scored,  k = 5, fill = NA, align = "right"),
      ra_roll  = rollmean(runs_allowed, k = 5, fill = NA, align = "right")
    )

  n_first <- nrow(first_half)
  if (n_first == 0) { cat(sprintf("  %s: no first-half rows, skipping\n", TEAM_NAME)); next }

  second_half <- schedule_preds %>%
    filter(Team == TEAM_NAME) %>%
    arrange(Date) %>%
    mutate(
      game_num      = row_number() + n_first,
      proj_cum_wins = max(first_half$cum_wins) + cumsum(pred_win_prob),
      proj_rs_roll  = rollmean(pred_runs_scored,  k = 5, fill = NA, align = "right"),
      proj_ra_roll  = rollmean(pred_runs_allowed, k = 5, fill = NA, align = "right")
    )

  n_second    <- nrow(second_half)
  total_games <- n_first + n_second
  deadline_x  <- n_first + 0.5

  add_deadline <- function(p, ymin, ymax) {
    p +
      annotate("rect", xmin = n_first, xmax = n_first + 1, ymin = ymin, ymax = ymax,
               fill = col_deadline, alpha = 0.08) +
      geom_vline(xintercept = deadline_x, color = col_deadline, linewidth = 0.7, linetype = "dashed") +
      annotate("text", x = deadline_x - 0.5, y = ymax, label = "Trade\ndeadline",
               hjust = 1, vjust = 1, size = 3, color = col_deadline, fontface = "italic")
  }

  # PANEL 1 — CUMULATIVE WINS (actual first half, projected second half)
  proj_wins_final <- if (n_second > 0) round(max(second_half$proj_cum_wins), 1) else max(first_half$cum_wins)
  y_max_wins <- max(max(first_half$cum_wins), proj_wins_final) * 1.08

  p1_base <- ggplot() +
    geom_line(data = first_half, aes(x = game_num, y = cum_wins, color = "Actual (first half)"), linewidth = 1.1)

  if (n_second > 0) {
    p1_base <- p1_base +
      geom_line(data = second_half, aes(x = game_num, y = proj_cum_wins, color = "Projected (rest of season)"),
                linewidth = 1.0, linetype = "dashed") +
      annotate("text", x = total_games + 0.8, y = proj_wins_final, label = proj_wins_final,
               color = col_proj, size = 3.5, hjust = 0, fontface = "bold")
  }

  p1 <- p1_base +
    annotate("text", x = n_first + 0.8, y = max(first_half$cum_wins), label = max(first_half$cum_wins),
             color = col_actual, size = 3.5, hjust = 0, fontface = "bold") +
    scale_color_manual(values = c("Actual (first half)" = col_actual,
                                  "Projected (rest of season)" = col_proj), name = NULL) +
    scale_x_continuous(breaks = seq(0, total_games, by = 20), expand = expansion(mult = c(0, 0.08))) +
    scale_y_continuous(breaks = seq(0, 120, by = 10)) +
    labs(
      title    = sprintf("%s — 2026 Season Projection: Cumulative Wins", TEAM_NAME),
      subtitle = sprintf("First half actual: %d wins in %d games  |  Projected rest of season: %.1f wins in %d games",
                         max(first_half$cum_wins), n_first, proj_wins_final - max(first_half$cum_wins), n_second),
      x = NULL, y = "Cumulative wins"
    ) +
    its_theme
  p1 <- add_deadline(p1, 0, y_max_wins)

  # PANEL 2 — RUNS SCORED ROLLING AVG
  y_max_rs <- max(first_half$rs_roll, second_half$proj_rs_roll, na.rm = TRUE) * 1.1
  y_min_rs <- max(0, min(first_half$rs_roll, second_half$proj_rs_roll, na.rm = TRUE) * 0.9)

  p2_base <- ggplot() +
    geom_line(data = first_half %>% filter(!is.na(rs_roll)),
              aes(x = game_num, y = rs_roll, color = "Actual (first half)"), linewidth = 1.0)
  if (n_second > 0) {
    p2_base <- p2_base +
      geom_line(data = second_half %>% filter(!is.na(proj_rs_roll)),
                aes(x = game_num, y = proj_rs_roll, color = "Projected (rest of season)"),
                linewidth = 1.0, linetype = "dashed")
  }
  p2 <- p2_base +
    scale_color_manual(values = c("Actual (first half)" = col_actual,
                                  "Projected (rest of season)" = col_proj), name = NULL) +
    scale_x_continuous(breaks = seq(0, total_games, by = 20), expand = expansion(mult = c(0, 0.07))) +
    coord_cartesian(ylim = c(y_min_rs, y_max_rs)) +
    labs(title = "Runs scored per game (5-game rolling avg)", x = NULL, y = "Runs scored") +
    its_theme
  p2 <- add_deadline(p2, y_min_rs, y_max_rs)

  # PANEL 3 — RUNS ALLOWED ROLLING AVG
  y_max_ra <- max(first_half$ra_roll, second_half$proj_ra_roll, na.rm = TRUE) * 1.1
  y_min_ra <- max(0, min(first_half$ra_roll, second_half$proj_ra_roll, na.rm = TRUE) * 0.9)

  p3_base <- ggplot() +
    geom_line(data = first_half %>% filter(!is.na(ra_roll)),
              aes(x = game_num, y = ra_roll, color = "Actual (first half)"), linewidth = 1.0)
  if (n_second > 0) {
    p3_base <- p3_base +
      geom_line(data = second_half %>% filter(!is.na(proj_ra_roll)),
                aes(x = game_num, y = proj_ra_roll, color = "Projected (rest of season)"),
                linewidth = 1.0, linetype = "dashed")
  }
  p3 <- p3_base +
    scale_color_manual(values = c("Actual (first half)" = col_actual,
                                  "Projected (rest of season)" = col_proj), name = NULL) +
    scale_x_continuous(breaks = seq(0, total_games, by = 20), expand = expansion(mult = c(0, 0.07))) +
    coord_cartesian(ylim = c(y_min_ra, y_max_ra)) +
    labs(title = "Runs allowed per game (5-game rolling avg)", x = "Game number (full season)", y = "Runs allowed") +
    its_theme
  p3 <- add_deadline(p3, y_min_ra, y_max_ra)

  # TRADE MOVES PANEL
  if (!is.null(trades_raw)) {
    excel_abbrev <- if (TEAM_NAME %in% names(rev_map)) rev_map[[TEAM_NAME]] else TEAM_NAME
    team_trades  <- trades_raw %>% filter(trimws(Team) == excel_abbrev) %>% arrange(date_of_trade)
    moves        <- build_moves(team_trades)
  } else {
    moves <- c("Trades file not available.")
  }
  moves_text <- paste0("Trade deadline moves — ", TEAM_NAME, " (August 3, 2026)\n",
                       paste(paste0("• ", moves), collapse = "\n"))

  p_moves <- ggplot() +
    annotate("text", x = 0.02, y = 0.5, label = moves_text, hjust = 0, vjust = 0.5,
             size = 3.3, color = "#333333", lineheight = 1.6) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#F8F8F8", color = "#DDDDDD", linewidth = 0.5),
          plot.margin = margin(8, 8, 8, 8)) +
    xlim(0, 1) + ylim(0, 1)

  final_plot <- (p1 / (p2 | p3) / p_moves) +
    plot_layout(heights = c(3, 2, 1.2)) +
    plot_annotation(
      caption = paste0(
        "Rolling averages use 5-game windows. First half shows actual results (real 2026 data through August 3).\n",
        "Second half shows the trained CatBoost model's per-game projections for the remaining schedule — ",
        "actual second-half results don't exist yet.\n",
        "Projection reflects the team's pre-deadline profile plus roster changes captured in the trade log — not a guarantee."
      )
    )

  out_file <- sprintf("%s%s_2026_projection.png", PANEL_DIR, TEAM_NAME)
  ggsave(out_file, final_plot, width = 18, height = 12, dpi = 200, bg = "white")
  cat(sprintf("  Saved %s\n", out_file))
}

cat("\nDone — team projection panels written to Team Projections\\\n")
