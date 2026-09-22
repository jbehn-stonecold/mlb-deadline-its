library(dplyr)

#  ADD MISSING (UNSCHEDULED MAKEUP) GAMES TO THE REMAINING SCHEDULE
#
#  Some teams show fewer than 162 total games (actual first half + scheduled
#  remainder) because a postponed game hasn't had a makeup date announced
#  yet -- it exists in neither the "already played" list nor the public
#  future schedule. Verified this is real (not a scraping bug): complete
#  historical seasons show the same occasional pattern (e.g. 2024 had
#  CLE/HOU both short by one), and every short team in 2026 pairs up
#  exactly with one other short team at an anomalously low head-to-head
#  meeting count for their matchup type (division/same-league/interleague)
#  -- exactly the signature of one unmade-up game between them.
#
#  Rather than guessing the outcome directly, this appends each missing
#  game to schedule_2026_remaining.csv (placed the day after the latest
#  currently-scheduled date) so 11_build_and_predict_2026_schedule picks it
#  up and runs it through the real trained CatBoost models like any other
#  scheduled game -- same feature engineering, same models, no shortcuts.
#
#  Run this BEFORE (re-)running 11_build_and_predict_2026_schedule.
#
#  Input/Output: Data\schedule_2026_remaining.csv (in place)

DATA_DIR <- "../data/"

schedule <- read.csv(paste0(DATA_DIR, "schedule_2026_remaining.csv"), stringsAsFactors = FALSE)
schedule$Date <- as.Date(schedule$Date)

makeup_date <- max(schedule$Date) + 1
cat(sprintf("Placing makeup games on %s (day after the latest scheduled date)\n", makeup_date))

# Missing-game pairs identified via head-to-head meeting-count analysis.
# First team listed gets home field (arbitrary -- the real host is unknown
# since this game was never officially rescheduled).
missing_pairs <- list(
  c("TBR", "COL"),
  c("NYY", "STL"),
  c("TEX", "SFG"),
  c("ARI", "SDP"),
  c("CHC", "LAD"),
  c("HOU", "TOR"),
  c("MIL", "PIT"),
  c("PHI", "WSN")
)

new_rows <- bind_rows(lapply(missing_pairs, function(pair) {
  home <- pair[1]; away <- pair[2]
  bind_rows(
    data.frame(Team = home, Date = makeup_date, Opp = away, is_home = 1L),
    data.frame(Team = away, Date = makeup_date, Opp = home, is_home = 0L)
  )
}))

cat(sprintf("Adding %d rows (%d makeup games) to the remaining schedule\n",
            nrow(new_rows), length(missing_pairs)))

schedule <- bind_rows(schedule, new_rows)

write.csv(schedule, paste0(DATA_DIR, "schedule_2026_remaining.csv"), row.names = FALSE)
cat("Saved updated schedule_2026_remaining.csv\n")
cat("Next: re-run 11_build_and_predict_2026_schedule_finished.R\n")
