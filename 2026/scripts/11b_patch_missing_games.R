library(dplyr)

#  PATCH MISSING (UNSCHEDULED MAKEUP) GAMES
#
#  Some teams show fewer than 162 total games (actual first half + scheduled
#  remainder) because a postponed game hasn't had a makeup date announced
#  yet -- it exists in neither the "already played" list nor the public
#  future schedule. Verified this is real (not a scraping bug) by checking
#  that complete historical seasons show the same occasional pattern (e.g.
#  2024 had CLE/HOU both short by one), and by confirming every short team
#  in 2026 pairs up exactly with one other short team at an anomalously low
#  head-to-head meeting count for their matchup type (division/same-league/
#  interleague), which is exactly the signature of one unmade-up game.
#
#  Since we can't know the real outcome of a game that hasn't been played
#  or even scheduled, this resolves each missing game by awarding the win to
#  whichever team has the higher model-projected rest-of-season win% --
#  the same win probability already driving every other rest-of-season
#  projection in this pipeline -- and updates both teams' full-season
#  totals accordingly so every team reaches its true 162-game slate.
#
#  Input/Output: Outputs\schedule_predictions_2026_team_summary.csv (in place)

OUT_DIR <- "../outputs/"

team_summary <- read.csv(paste0(OUT_DIR, "schedule_predictions_2026_team_summary.csv"),
                         stringsAsFactors = FALSE)

# Missing-game pairs identified via head-to-head meeting-count analysis
# (each pair was short exactly one game relative to the expected count for
# their matchup type -- division: 13, same-league: 6, interleague: 3).
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

team_summary$synthetic_game_opponent <- NA_character_
team_summary$synthetic_game_result   <- NA_character_

for (pair in missing_pairs) {
  a <- pair[1]; b <- pair[2]
  row_a <- which(team_summary$Team == a)
  row_b <- which(team_summary$Team == b)

  if (length(row_a) != 1 || length(row_b) != 1) {
    cat(sprintf("  WARNING: could not find both %s and %s in team_summary -- skipping.\n", a, b))
    next
  }

  pct_a <- team_summary$proj_post_win_pct[row_a]
  pct_b <- team_summary$proj_post_win_pct[row_b]
  winner_row <- if (pct_a >= pct_b) row_a else row_b
  loser_row  <- if (pct_a >= pct_b) row_b else row_a
  winner_team <- team_summary$Team[winner_row]
  loser_team  <- team_summary$Team[loser_row]

  cat(sprintf("  %s (%.3f) vs %s (%.3f) -> %s wins\n", a, pct_a, b, pct_b, winner_team))

  team_summary$remaining_games[winner_row]      <- team_summary$remaining_games[winner_row] + 1
  team_summary$proj_wins_remaining[winner_row]  <- team_summary$proj_wins_remaining[winner_row] + 1
  team_summary$synthetic_game_opponent[winner_row] <- loser_team
  team_summary$synthetic_game_result[winner_row]   <- "win"

  team_summary$remaining_games[loser_row]       <- team_summary$remaining_games[loser_row] + 1
  team_summary$synthetic_game_opponent[loser_row]  <- winner_team
  team_summary$synthetic_game_result[loser_row]    <- "loss"
}

# Recompute every field derived from remaining_games / proj_wins_remaining
team_summary <- team_summary %>%
  mutate(
    proj_post_win_pct        = round(proj_wins_remaining / remaining_games, 3),
    proj_full_season_games   = actual_games_first_half + remaining_games,
    proj_full_season_wins    = round(actual_wins_first_half + proj_wins_remaining, 1),
    proj_full_season_losses  = round(proj_full_season_games - proj_full_season_wins, 1),
    proj_full_season_win_pct = round(proj_full_season_wins / proj_full_season_games, 3),
    win_pct_delta            = round(proj_post_win_pct - pre_win_pct, 3)
  ) %>%
  arrange(desc(proj_full_season_wins))

n_162 <- sum((team_summary$actual_games_first_half + team_summary$remaining_games) == 162)
cat(sprintf("\nTeams now at 162 total games: %d / 30\n", n_162))

write.csv(team_summary, paste0(OUT_DIR, "schedule_predictions_2026_team_summary.csv"), row.names = FALSE)
cat("Saved patched schedule_predictions_2026_team_summary.csv\n")
