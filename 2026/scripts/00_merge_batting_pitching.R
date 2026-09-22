library(dplyr)

#  MERGE TEAM BATTING + PITCHING GAME LOGS
#
#  Replaces the old CB.R approach (archived), which built stathead_team.csv
#  via cbind() -- a straight side-by-side column bind that silently
#  misaligns every row if the two source files don't have exactly the same
#  rows in exactly the same order. That's a real risk here: the batting and
#  pitching scrapers filter on different WPA thresholds (1.9 vs 2.1), so
#  they aren't guaranteed to return the same game set.
#
#  This does a real key-based join on Team + Date (+ a within-day sequence
#  number to disambiguate doubleheaders), and reports any games present in
#  one file but not the other instead of silently dropping/misaligning them.
#
#  Output columns use a plain, explicit "_pit" suffix for any pitching-side
#  column whose name collides with a batting-side column (e.g. pitching's
#  hits-allowed "H" becomes "H_pit" to distinguish from batting's own "H").
#  01_prepare_and_engineer's column selection matches this convention.

DATA_DIR <- "../data/"

batting  <- read.csv(paste0(DATA_DIR, "stathead_team_batting.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE)
pitching <- read.csv(paste0(DATA_DIR, "stathead_team_pitching.csv"),
                     stringsAsFactors = FALSE, check.names = FALSE)

batting$Date  <- as.Date(batting$Date)
pitching$Date <- as.Date(pitching$Date)

# Doubleheaders: disambiguate same Team+Date with a within-day sequence
# number. Both scrapes are ordered by date (order_by=date), so a team's
# games sharing a date line up in the same chronological order.
batting <- batting %>%
  arrange(Team, Date) %>%
  group_by(Team, Date) %>%
  mutate(game_seq = row_number()) %>%
  ungroup()

pitching <- pitching %>%
  arrange(Team, Date) %>%
  group_by(Team, Date) %>%
  mutate(game_seq = row_number()) %>%
  ungroup()

# Report games present in one file but not the other, instead of silently
# misaligning or dropping them the way the old cbind() approach could.
batting_key  <- paste(batting$Team, batting$Date, batting$game_seq)
pitching_key <- paste(pitching$Team, pitching$Date, pitching$game_seq)
only_batting  <- setdiff(batting_key, pitching_key)
only_pitching <- setdiff(pitching_key, batting_key)

cat(sprintf("Batting rows: %d | Pitching rows: %d\n", nrow(batting), nrow(pitching)))
cat(sprintf("Games in batting but not pitching: %d\n", length(only_batting)))
cat(sprintf("Games in pitching but not batting: %d\n", length(only_pitching)))
if (length(only_batting) > 0) {
  cat("  e.g.:", paste(head(only_batting, 5), collapse = " | "), "\n")
}
if (length(only_pitching) > 0) {
  cat("  e.g.:", paste(head(only_pitching, 5), collapse = " | "), "\n")
}

team <- inner_join(
  batting, pitching,
  by = c("Team", "Date", "game_seq"),
  suffix = c("", "_pit")
)

cat(sprintf("Merged rows: %d\n", nrow(team)))

team$teamclass <- paste(team$Team, team$scrape_year)

write.csv(team, paste0(DATA_DIR, "stathead_team.csv"), row.names = FALSE)
cat("Saved stathead_team.csv\n")
