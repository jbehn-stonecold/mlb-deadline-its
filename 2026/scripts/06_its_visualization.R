library(dplyr)
library(ggplot2)
library(patchwork)
library(zoo)
library(lubridate)


#  ITS PANEL VISUALIZATION v3 — FULL SEASON WITH AVERAGE REFERENCE LINES
#
#  Shows the full 2026 season split at the trade deadline:
#    Left of deadline:  actual first-half game-by-game results
#    Right of deadline: actual second half vs projected counterfactual
#
#  RS and RA panels show 5-game rolling averages with flat horizontal
#  reference lines for the second-half projected and actual averages.
#  No confidence bands.


TEAM_NAME   <- "NYM"   # <-- change this to any model team abbreviation
# Manually-maintained trade-deadline tracker; not produced by the scrapers,
# supply your own copy at data/trades_2026.xlsx
TRADES_FILE <- "../data/trades_2026.xlsx"

# DYNAMIC TRADE LOADING
# Reads trades_2026.xlsx and builds DEADLINE_MOVES automatically for TEAM_NAME.
# Handles abbreviation differences between the Excel file and model abbreviations.

library(readxl)

# Mapping from Excel abbreviations to model abbreviations
abbrev_map <- c(
  "TB"  = "TBR",
  "KC"  = "KCR",
  "SD"  = "SDP",
  "SF"  = "SFG",
  "ATH" = "OAK",
  "CWS" = "CHW",
  "WSH" = "WSN"
)

# Reverse map: model abbrev -> Excel abbrev (for looking up the team's trades)
rev_map <- setNames(names(abbrev_map), abbrev_map)

# Determine what abbreviation the Excel file uses for TEAM_NAME
excel_abbrev <- if (TEAM_NAME %in% names(rev_map)) rev_map[[TEAM_NAME]] else TEAM_NAME

# Load trades — if the tracker hasn't been supplied yet, still draw the panel
# with a placeholder note in the moves box (re-run once the file exists)
HAS_TRADES <- file.exists(TRADES_FILE)
if (HAS_TRADES) {
  trades_raw <- read_excel(TRADES_FILE, sheet = "Sheet1")
  colnames(trades_raw) <- c("Team", "Players_added", "pA_pos",
                             "Players_given", "pG_pos",
                             "Trade_count", "date_of_trade", "notes")

  # Filter to this team's rows
  team_trades <- trades_raw %>%
    filter(trimws(Team) == excel_abbrev) %>%
    arrange(date_of_trade)
} else {
  cat(sprintf("%s not found — moves panel will show a placeholder.\n", TRADES_FILE))
}

# Build bullet lines
build_moves <- function(trades_df) {
  if (nrow(trades_df) == 0) {
    return(c("No recorded trades at the 2026 deadline."))
  }

  lines <- character(0)
  for (i in seq_len(nrow(trades_df))) {
    row <- trades_df[i, ]

    added   <- trimws(row$Players_added)
    added_p <- trimws(row$pA_pos)
    given   <- trimws(row$Players_given)
    given_p <- trimws(row$pG_pos)

    # Build a single combined line per trade
    acq_str  <- if (!is.na(added) && tolower(added) != "cash")
                  sprintf("Acquired: %s (%s)", added, added_p) else NULL
    trd_str  <- if (!is.na(given) && tolower(given) != "cash")
                  sprintf("Traded: %s (%s)", given, given_p) else NULL

    trade_line <- paste(c(acq_str, trd_str), collapse = " | ")
    if (nchar(trade_line) > 0) lines <- c(lines, trade_line)
  }
  return(lines)
}

if (HAS_TRADES) {
  DEADLINE_MOVES <- build_moves(team_trades)
  cat(sprintf("Loaded %d trade(s) for %s:\n", nrow(team_trades), TEAM_NAME))
} else {
  DEADLINE_MOVES <- "Trade tracker (data/trades_2026.xlsx) not yet added \u2014 moves pending."
}
cat(paste0("  ", DEADLINE_MOVES, collapse = "\n"), "\n\n")

# 0. LOAD AND PREPARE FIRST HALF DATA
cat("Loading first half data from stathead_team.csv...\n")

raw <- read.csv("../data/stathead_team.csv", stringsAsFactors = FALSE)

raw <- raw %>%
  mutate(
    Team   = dplyr::recode(Team, "FLA"="MIA", "TBD"="TBR", "ATH"="OAK"),
    Date   = as.Date(Date),
    Season = scrape_year
  )

score_match <- regmatches(raw$Result, regexpr("[0-9]+-[0-9]+", raw$Result))
score_split <- strsplit(score_match, "-")
raw$runs_scored  <- as.integer(sapply(score_split, `[`, 1))
raw$runs_allowed <- as.integer(sapply(score_split, `[`, 2))
raw$win          <- as.integer(substr(raw$Result, 1, 1) == "W")

first_half <- raw %>%
  filter(Team == TEAM_NAME, Season == 2026, Date <= as.Date("2026-08-03")) %>%
  arrange(Date) %>%
  mutate(
    game_num  = row_number(),
    half      = "First half",
    cum_wins  = cumsum(win),
    cum_rd    = cumsum(runs_scored - runs_allowed),
    rs_roll   = rollmean(runs_scored,  k = 5, fill = NA, align = "right"),
    ra_roll   = rollmean(runs_allowed, k = 5, fill = NA, align = "right")
  )

n_first <- nrow(first_half)

# 1. LOAD AND PREPARE SECOND HALF DATA
post <- read.csv("../outputs/predictions_2026_game_level.csv", stringsAsFactors = FALSE)
post$Date <- as.Date(post$Date)

second_half <- post %>%
  filter(Team == TEAM_NAME) %>%
  arrange(Date) %>%
  mutate(
    game_num      = row_number() + n_first,
    half          = "Second half",
    cum_wins      = max(first_half$cum_wins) + cumsum(win),
    proj_cum_wins = max(first_half$cum_wins) + cumsum(pred_win_prob),
    cum_rd        = max(first_half$cum_rd)   + cumsum(runs_scored - runs_allowed),
    rs_roll       = rollmean(runs_scored,        k = 5, fill = NA, align = "right"),
    ra_roll       = rollmean(runs_allowed,       k = 5, fill = NA, align = "right"),
    proj_rs_roll  = rollmean(pred_runs_scored,   k = 5, fill = NA, align = "right"),
    proj_ra_roll  = rollmean(pred_runs_allowed,  k = 5, fill = NA, align = "right")
  )

# Second half averages — used for flat reference lines
avg_actual_rs  <- mean(second_half$runs_scored,       na.rm = TRUE)
avg_proj_rs    <- mean(second_half$pred_runs_scored,  na.rm = TRUE)
avg_actual_ra  <- mean(second_half$runs_allowed,      na.rm = TRUE)
avg_proj_ra    <- mean(second_half$pred_runs_allowed, na.rm = TRUE)

n_second    <- nrow(second_half)
total_games <- n_first + n_second
deadline_x  <- n_first + 0.5

# 2. THEME
col_actual   <- "#1D9E75"
col_proj     <- "#378ADD"
col_deadline <- "#D85A30"
col_grid     <- "#EEEEEE"
col_avg_act  <- "#1D9E75"   # solid teal for actual average line
col_avg_proj <- "#378ADD"   # solid blue for projected average line

its_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#555555", size = 10),
    axis.title       = element_text(color = "#333333", size = 11),
    plot.title       = element_text(color = "#111111", size = 13,
                                    face = "bold", hjust = 0),
    plot.subtitle    = element_text(color = "#666666", size = 10,
                                    hjust = 0, margin = margin(b = 6)),
    legend.position  = "top",
    legend.text      = element_text(color = "#444444", size = 10),
    legend.key.size  = unit(0.8, "lines"),
    plot.margin      = margin(t = 10, r = 20, b = 6, l = 10)
  )

# Helper to add deadline annotation
add_deadline <- function(p, ymin, ymax) {
  p +
    annotate("rect",
             xmin = n_first, xmax = n_first + 1,
             ymin = ymin, ymax = ymax,
             fill = col_deadline, alpha = 0.08) +
    geom_vline(xintercept = deadline_x,
               color = col_deadline, linewidth = 0.7, linetype = "dashed") +
    annotate("text",
             x = deadline_x - 0.5, y = ymax,
             label = "Trade\ndeadline", hjust = 1, vjust = 1,
             size = 3, color = col_deadline, fontface = "italic")
}

# 3. PANEL 1 — CUMULATIVE WINS
actual_wins_final <- max(second_half$cum_wins)
proj_wins_final   <- round(max(second_half$proj_cum_wins))   # whole wins, matching the standings charts
win_diff          <- actual_wins_final - proj_wins_final
diff_label        <- ifelse(win_diff >= 0, paste0("+", win_diff), as.character(win_diff))
diff_color        <- ifelse(win_diff >= 0, col_actual, col_deadline)

y_max_wins <- max(actual_wins_final, proj_wins_final) * 1.05

p1_base <- ggplot() +

  # First half actual
  geom_line(data = first_half,
            aes(x = game_num, y = cum_wins, color = "Actual"),
            linewidth = 1.1) +

  # Second half projected
  geom_line(data = second_half,
            aes(x = game_num, y = proj_cum_wins, color = "Projected"),
            linewidth = 1.0, linetype = "dashed") +

  # Second half actual
  geom_line(data = second_half,
            aes(x = game_num, y = cum_wins, color = "Actual"),
            linewidth = 1.2) +

  # Final value labels
  annotate("text", x = total_games + 0.8, y = actual_wins_final,
           label = actual_wins_final, color = col_actual,
           size = 3.5, hjust = 0, fontface = "bold") +
  annotate("text", x = total_games + 0.8, y = proj_wins_final,
           label = proj_wins_final, color = col_proj,
           size = 3.5, hjust = 0) +

  scale_color_manual(values = c("Actual" = col_actual, "Projected" = col_proj),
                     name = NULL) +
  scale_x_continuous(breaks = seq(0, total_games, by = 20),
                     expand = expansion(mult = c(0, 0.06))) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title    = sprintf("%s — 2026 Full Season: Cumulative Wins", TEAM_NAME),
    subtitle = sprintf(
      "Full-season actual: %d  |  Projected without deadline moves: %d  |  Impact: %s wins",
      actual_wins_final, as.integer(proj_wins_final), diff_label),
    x = NULL, y = "Cumulative wins"
  ) +
  its_theme +
  theme(plot.subtitle = element_text(color = diff_color, face = "bold"))

p1 <- add_deadline(p1_base, 0, y_max_wins)

# 4. PANEL 2 — RUNS SCORED
rs_impact  <- round(avg_actual_rs - avg_proj_rs, 2)
rs_label   <- ifelse(rs_impact >= 0, paste0("+", rs_impact), as.character(rs_impact))
rs_color   <- ifelse(rs_impact >= 0, col_actual, col_deadline)

# y axis range — driven by the rolling average lines
y_max_rs <- max(first_half$rs_roll, second_half$rs_roll, na.rm = TRUE) * 1.08
y_min_rs <- max(0, min(first_half$rs_roll, second_half$rs_roll,
                        second_half$proj_rs_roll, na.rm = TRUE) * 0.92)

p2_base <- ggplot() +

  # First half actual rolling avg
  geom_line(data = first_half %>% filter(!is.na(rs_roll)),
            aes(x = game_num, y = rs_roll, color = "Actual"),
            linewidth = 1.0) +

  # Second half projected rolling avg
  geom_line(data = second_half %>% filter(!is.na(proj_rs_roll)),
            aes(x = game_num, y = proj_rs_roll, color = "Projected"),
            linewidth = 0.9, linetype = "dashed") +

  # Second half actual rolling avg
  geom_line(data = second_half %>% filter(!is.na(rs_roll)),
            aes(x = game_num, y = rs_roll, color = "Actual"),
            linewidth = 1.1) +

  # Flat average reference lines — second half only
  annotate("segment",
           x = n_first + 1, xend = total_games,
           y = avg_actual_rs, yend = avg_actual_rs,
           color = col_avg_act, linewidth = 0.8, linetype = "solid", alpha = 0.5) +
  annotate("segment",
           x = n_first + 1, xend = total_games,
           y = avg_proj_rs, yend = avg_proj_rs,
           color = col_avg_proj, linewidth = 0.8, linetype = "solid", alpha = 0.5) +

  # Labels for average lines at right edge
  annotate("text", x = total_games + 0.5, y = avg_actual_rs,
           label = sprintf("%.2f", avg_actual_rs),
           color = col_actual, size = 3, hjust = 0, fontface = "bold") +
  annotate("text", x = total_games + 0.5, y = avg_proj_rs,
           label = sprintf("%.2f", avg_proj_rs),
           color = col_proj, size = 3, hjust = 0) +

  scale_color_manual(values = c("Actual" = col_actual, "Projected" = col_proj),
                     name = NULL) +
  scale_x_continuous(breaks = seq(0, total_games, by = 20),
                     expand = expansion(mult = c(0, 0.07))) +
  coord_cartesian(ylim = c(y_min_rs, y_max_rs)) +
  labs(
    title    = "Runs scored per game (5-game rolling avg)",
    subtitle = sprintf("Second half avg — actual: %.2f  |  projected: %.2f  |  impact: %s R/G",
                       avg_actual_rs, avg_proj_rs, rs_label),
    x = NULL, y = "Runs scored"
  ) +
  its_theme +
  theme(plot.subtitle = element_text(color = rs_color))

p2 <- add_deadline(p2_base, y_min_rs, y_max_rs)

# 5. PANEL 3 — RUNS ALLOWED
ra_impact <- round(avg_actual_ra - avg_proj_ra, 2)
ra_label  <- ifelse(ra_impact >= 0, paste0("+", ra_impact), as.character(ra_impact))
ra_color  <- ifelse(ra_impact < 0, col_actual, col_deadline)  # negative = better pitching

y_max_ra <- max(first_half$ra_roll, second_half$ra_roll, na.rm = TRUE) * 1.08
y_min_ra <- max(0, min(first_half$ra_roll, second_half$ra_roll,
                        second_half$proj_ra_roll, na.rm = TRUE) * 0.92)

p3_base <- ggplot() +

  # First half actual rolling avg
  geom_line(data = first_half %>% filter(!is.na(ra_roll)),
            aes(x = game_num, y = ra_roll, color = "Actual"),
            linewidth = 1.0) +

  # Second half projected rolling avg
  geom_line(data = second_half %>% filter(!is.na(proj_ra_roll)),
            aes(x = game_num, y = proj_ra_roll, color = "Projected"),
            linewidth = 0.9, linetype = "dashed") +

  # Second half actual rolling avg
  geom_line(data = second_half %>% filter(!is.na(ra_roll)),
            aes(x = game_num, y = ra_roll, color = "Actual"),
            linewidth = 1.1) +

  # Flat average reference lines — second half only
  annotate("segment",
           x = n_first + 1, xend = total_games,
           y = avg_actual_ra, yend = avg_actual_ra,
           color = col_avg_act, linewidth = 0.8, linetype = "solid", alpha = 0.5) +
  annotate("segment",
           x = n_first + 1, xend = total_games,
           y = avg_proj_ra, yend = avg_proj_ra,
           color = col_avg_proj, linewidth = 0.8, linetype = "solid", alpha = 0.5) +

  # Labels for average lines at right edge
  annotate("text", x = total_games + 0.5, y = avg_actual_ra,
           label = sprintf("%.2f", avg_actual_ra),
           color = col_actual, size = 3, hjust = 0, fontface = "bold") +
  annotate("text", x = total_games + 0.5, y = avg_proj_ra,
           label = sprintf("%.2f", avg_proj_ra),
           color = col_proj, size = 3, hjust = 0) +

  scale_color_manual(values = c("Actual" = col_actual, "Projected" = col_proj),
                     name = NULL) +
  scale_x_continuous(breaks = seq(0, total_games, by = 20),
                     expand = expansion(mult = c(0, 0.07))) +
  coord_cartesian(ylim = c(y_min_ra, y_max_ra)) +
  labs(
    title    = "Runs allowed per game (5-game rolling avg)",
    subtitle = sprintf("Second half avg — actual: %.2f  |  projected: %.2f  |  impact: %s R/G",
                       avg_actual_ra, avg_proj_ra, ra_label),
    x = "Game number (full season)", y = "Runs allowed"
  ) +
  its_theme +
  theme(plot.subtitle = element_text(color = ra_color))

p3 <- add_deadline(p3_base, y_min_ra, y_max_ra)

# 6. DEADLINE MOVES PANEL
moves_text <- paste0(
  "Trade deadline moves \u2014 ", TEAM_NAME, " (August 3, 2026)\n",
  paste(paste0("\u2022 ", DEADLINE_MOVES), collapse = "\n")
)

p_moves <- ggplot() +
  annotate("text",
           x = 0.02, y = 0.5,
           label      = moves_text,
           hjust      = 0, vjust = 0.5,
           size       = 3.3,
           color      = "#333333",
           lineheight = 1.6) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F8F8F8",
                                   color = "#DDDDDD",
                                   linewidth = 0.5),
    plot.margin = margin(8, 8, 8, 8)
  ) +
  xlim(0, 1) + ylim(0, 1)

# 7. COMBINE WITH PATCHWORK
# Moves box grows with the number of trades so long lists aren't clipped
moves_h <- 0.8 + 0.3 * length(DEADLINE_MOVES)

final_plot <- (p1 / (p2 | p3) / p_moves) +
  plot_layout(heights = c(3, 2, moves_h)) +
  plot_annotation(
    caption = paste0(
      "Rolling averages use 5-game windows. Flat lines show second-half averages for actual and projected.\n",
      "First half shows actual results. Deadline line marks August 3.\n",
      "Projection reflects historical peer expectation \u2014 not a pure causal estimate of deadline impact."
    )
  )

#8. SAVE
out_file <- sprintf("../model-validation/%s_2026_its_panel_FINAL.png", TEAM_NAME)
ggsave(out_file, final_plot, width = 18, height = 10 + 1.4 * moves_h, dpi = 200, bg = "white")
