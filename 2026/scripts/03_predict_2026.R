library(dplyr)

#  SCORE THE 2026 TRADE-DEADLINE PROJECTIONS AGAINST ACTUAL RESULTS
#
#  Uses the per-game predictions saved at the Aug 3 deadline by
#  11_build_and_predict_2026_schedule.R (the ones behind the charts in
#  ../predictions/) — the models are NOT re-run here, so this scores exactly
#  what was projected at the deadline.
#
#  Each actual post-deadline game is matched to its saved prediction on
#  Team + Date + Opp (+ doubleheader order). Games postponed after the
#  deadline and made up on a different date are matched to the earliest
#  unused scheduled game between the same two teams. Scheduled games
#  that were never played (e.g. the placeholder "makeup" games 11b added on
#  Sep 28, or a cancelled game) are listed and left out.
#
#  Input:  ../outputs/schedule_predictions_2026_game_level.csv (deadline predictions)
#          ../data/model_features.csv (actual 2026 post-deadline results)
#  Output: ../outputs/predictions_2026_game_level.csv
#          ../outputs/predictions_2026_team_summary.csv

# 0. LOAD ACTUAL POST-DEADLINE RESULTS
df      <- read.csv("../data/model_features.csv", stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)

actual <- df %>%
  filter(Season == 2026) %>%
  arrange(Team, Date) %>%
  group_by(Team, Date, Opp) %>% mutate(dh = row_number()) %>% ungroup() %>%
  mutate(.row = row_number())
cat("2026 post-deadline games played (team rows):", nrow(actual), "\n")

# 1. LOAD SAVED DEADLINE PREDICTIONS
saved <- read.csv("../outputs/schedule_predictions_2026_game_level.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Team, Date) %>%
  group_by(Team, Date, Opp) %>% mutate(dh = row_number()) %>% ungroup() %>%
  mutate(.srow = row_number()) %>%
  select(.srow, Team, Date, Opp, dh, is_home,
         pred_runs_scored, pred_runs_allowed, pred_win_prob)
cat("Saved deadline predictions (team rows):", nrow(saved), "\n")

# 2. MATCH EACH PLAYED GAME TO ITS SAVED PREDICTION
# Pass 1: same Team, Date, Opp and doubleheader order
match1 <- actual %>%
  select(.row, Team, Date, Opp, dh) %>%
  inner_join(saved %>% select(.srow, Team, Date, Opp, dh), by = c("Team", "Date", "Opp", "dh"))

# Pass 2: rescheduled games — earliest unused saved game between the same
# two teams (is_home in model_features.csv is always 0, so it can't be used)
left_act   <- actual %>% filter(!.row %in% match1$.row) %>% arrange(Date)
used_srows <- match1$.srow
match2 <- list()
for (i in seq_len(nrow(left_act))) {
  a <- left_act[i, ]
  cand <- saved %>%
    filter(Team == a$Team, Opp == a$Opp, !.srow %in% used_srows) %>%
    arrange(Date)
  if (nrow(cand) == 0) stop(sprintf("No saved prediction for %s vs %s on %s", a$Team, a$Opp, a$Date))
  used_srows <- c(used_srows, cand$.srow[1])
  match2[[i]] <- data.frame(.row = a$.row, .srow = cand$.srow[1])
  cat(sprintf("  Rescheduled: %s vs %s played %s, matched to deadline game scheduled %s\n",
              a$Team, a$Opp, a$Date, cand$Date[1]))
}
matches <- bind_rows(match1 %>% select(.row, .srow), bind_rows(match2))
stopifnot(!anyDuplicated(matches$.row), !anyDuplicated(matches$.srow))

unplayed <- saved %>% filter(!.srow %in% matches$.srow)
cat(sprintf("Scheduled at the deadline but never played (team rows): %d\n", nrow(unplayed)))
if (nrow(unplayed) > 0) print(as.data.frame(unplayed %>% select(Team, Date, Opp)), row.names = FALSE)

pred_df <- actual %>%
  inner_join(matches, by = ".row") %>%
  inner_join(saved %>% select(.srow, pred_runs_scored, pred_runs_allowed, pred_win_prob), by = ".srow") %>%
  select(-.row, -.srow, -dh) %>%
  arrange(Team, Date)
stopifnot(nrow(pred_df) == nrow(actual))

pyth_win_prob <- function(rs, ra, exp = 1.83) rs^exp / (rs^exp + ra^exp)

pred_df$pred_win    <- as.integer(pred_df$pred_win_prob >= 0.5)
pred_df$pred_win_rd <- as.integer(pred_df$pred_runs_scored > pred_df$pred_runs_allowed)

cat(sprintf("\nRD vs W/L agreement: %d / %d games (%.1f%%)\n",
            sum(pred_df$pred_win == pred_df$pred_win_rd, na.rm = TRUE),
            nrow(pred_df),
            100 * mean(pred_df$pred_win == pred_df$pred_win_rd, na.rm = TRUE)))

# 5. AGGREGATE TO TEAM TOTALS

# RMSE from held-out test set — used to compute intervals
# From 02b_test_results.csv (2025 held-out season, n = 1590 games)
RMSE_RS <- 3.2059
RMSE_RA <- 3.2074

# Z-scores for 90%, 95%, 99% confidence intervals
Z90 <- 1.645
Z95 <- 1.960
Z99 <- 2.576

# Significance stars helper — based on whether CI excludes zero
# Uses 90/95/99% CI on the IMPACT (actual minus projected)
# *** p<=0.01  ** p<=0.05  * p<=0.10  (blank) not significant
impact_stars <- function(impact, sd_proj, n_games = NULL, type = "wins") {
  # For wins: sd_proj is the Bernoulli SD of projected wins
  # For RS/RA per game: sd_proj is RMSE/sqrt(n)
  # CI on impact = impact ± Z * sd_proj (actual is fixed, uncertainty in projection)
  stars_fn <- function(z_val) {
    lo <- impact - z_val * sd_proj
    hi <- impact + z_val * sd_proj
    !(lo <= 0 & hi >= 0)  # TRUE = CI excludes zero = significant
  }
  dplyr::case_when(
    stars_fn(Z99) ~ "***",
    stars_fn(Z95) ~ "**",
    stars_fn(Z90) ~ "*",
    TRUE          ~ ""
  )
}

team_summary <- pred_df %>%
  group_by(Team) %>%
  summarise(
    games_played       = n(),
    actual_wins        = sum(win,           na.rm = TRUE),
    actual_losses      = games_played - actual_wins,
    actual_win_pct     = round(actual_wins / games_played, 3),
    actual_rs_total    = sum(runs_scored,   na.rm = TRUE),
    actual_ra_total    = sum(runs_allowed,  na.rm = TRUE),
    actual_rs_per_game = round(actual_rs_total / games_played, 2),
    actual_ra_per_game = round(actual_ra_total / games_played, 2),
    actual_run_diff    = actual_rs_total - actual_ra_total,
    proj_wins          = round(sum(pred_win_prob,     na.rm = TRUE)),
    proj_losses        = games_played - proj_wins,
    proj_win_pct       = round(proj_wins / games_played, 3),
    proj_rs_total      = round(sum(pred_runs_scored,  na.rm = TRUE), 1),
    proj_ra_total      = round(sum(pred_runs_allowed, na.rm = TRUE), 1),
    proj_rs_per_game   = round(proj_rs_total / games_played, 2),
    proj_ra_per_game   = round(proj_ra_total / games_played, 2),
    proj_run_diff      = round(proj_rs_total - proj_ra_total, 1),
    proj_wins_rd       = sum(pred_win_rd,   na.rm = TRUE),
    proj_win_pct_rd    = round(proj_wins_rd / games_played, 3),
    impact_wins        = actual_wins - proj_wins,
    impact_wins_rd     = actual_wins - proj_wins_rd,
    impact_win_pct     = round(actual_win_pct - proj_win_pct, 3),
    impact_win_pct_rd  = round(actual_win_pct - proj_win_pct_rd, 3),
    impact_rs_total    = round(actual_rs_total - proj_rs_total, 1),
    impact_ra_total    = round(actual_ra_total - proj_ra_total, 1),
    impact_rs_per_game = round(actual_rs_per_game - proj_rs_per_game, 2),
    impact_ra_per_game = round(actual_ra_per_game - proj_ra_per_game, 2),
    impact_run_diff    = round(actual_run_diff - proj_run_diff, 1),

    # CONFIDENCE INTERVALS — 90% PRIMARY (shown on chart)
    # Wins: Bernoulli variance SD = sqrt(sum(p*(1-p)))
    proj_wins_sd       = round(sqrt(sum(pred_win_prob * (1 - pred_win_prob),
                                        na.rm = TRUE)), 2),
    proj_wins_ci90_lo  = round(proj_wins - Z90 * proj_wins_sd),
    proj_wins_ci90_hi  = round(proj_wins + Z90 * proj_wins_sd),
    proj_wins_ci95_lo  = round(proj_wins - Z95 * proj_wins_sd),
    proj_wins_ci95_hi  = round(proj_wins + Z95 * proj_wins_sd),
    proj_wins_ci99_lo  = round(proj_wins - Z99 * proj_wins_sd),
    proj_wins_ci99_hi  = round(proj_wins + Z99 * proj_wins_sd),
    # Keep ci_lo/hi at 90% for chart labels
    proj_wins_ci_lo    = proj_wins_ci90_lo,
    proj_wins_ci_hi    = proj_wins_ci90_hi,

    # RS/G CI: RMSE / sqrt(n)
    rs_ci_margin_90    = round(Z90 * RMSE_RS / sqrt(games_played), 2),
    rs_ci_margin_95    = round(Z95 * RMSE_RS / sqrt(games_played), 2),
    rs_ci_margin_99    = round(Z99 * RMSE_RS / sqrt(games_played), 2),
    proj_rs_pg_ci_lo   = round(proj_rs_per_game - rs_ci_margin_90, 2),
    proj_rs_pg_ci_hi   = round(proj_rs_per_game + rs_ci_margin_90, 2),

    # RA/G CI
    ra_ci_margin_90    = round(Z90 * RMSE_RA / sqrt(games_played), 2),
    ra_ci_margin_95    = round(Z95 * RMSE_RA / sqrt(games_played), 2),
    ra_ci_margin_99    = round(Z99 * RMSE_RA / sqrt(games_played), 2),
    proj_ra_pg_ci_lo   = round(proj_ra_per_game - ra_ci_margin_90, 2),
    proj_ra_pg_ci_hi   = round(proj_ra_per_game + ra_ci_margin_90, 2),

    # Single-game prediction interval (95%)
    rs_pi_margin       = round(Z95 * RMSE_RS, 2),
    ra_pi_margin       = round(Z95 * RMSE_RA, 2),

    .groups = "drop"
  ) %>%
  # ── FORECAST COVERAGE STARS ───────────────────────────────────────────────
  # Stars = forecast coverage accuracy (how close actual was to projection).
  # *** = actual within 90% CI — tightest band, best prediction
  # **  = actual within 95% CI but outside 90% CI
  # *   = actual within 99% CI but outside 95% CI
  # (blank) = actual outside 99% CI — model significantly missed this team
  mutate(
    wins_se = proj_wins_sd,
    rs_se   = rs_ci_margin_90 / Z90,
    ra_se   = ra_ci_margin_90 / Z90,
    wins_stars = case_when(
      abs(impact_wins) <= Z90 * wins_se ~ "***",
      abs(impact_wins) <= Z95 * wins_se ~ "**",
      abs(impact_wins) <= Z99 * wins_se ~ "*",
      TRUE ~ ""
    ),
    rs_stars = case_when(
      abs(impact_rs_per_game) <= Z90 * rs_se ~ "***",
      abs(impact_rs_per_game) <= Z95 * rs_se ~ "**",
      abs(impact_rs_per_game) <= Z99 * rs_se ~ "*",
      TRUE ~ ""
    ),
    ra_stars = case_when(
      abs(impact_ra_per_game) <= Z90 * ra_se ~ "***",
      abs(impact_ra_per_game) <= Z95 * ra_se ~ "**",
      abs(impact_ra_per_game) <= Z99 * ra_se ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(desc(impact_wins))

# 6. PYTHAGOREAN SANITY CHECK
team_summary <- team_summary %>%
  mutate(
    pyth_proj_win_pct = round(pyth_win_prob(proj_rs_per_game, proj_ra_per_game), 3),
    pyth_vs_model_gap = round(proj_win_pct - pyth_proj_win_pct, 3)
  )

# 7. PRINT RESULTS

cat("── Win Impact (W/L model) ──\n")
team_summary %>%
  select(Team, games_played, proj_wins, actual_wins, impact_wins,
         proj_win_pct, actual_win_pct, impact_win_pct) %>%
  arrange(desc(impact_wins)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Runs Scored Impact ──\n")
team_summary %>%
  select(Team, proj_rs_per_game, actual_rs_per_game, impact_rs_per_game,
         proj_rs_total, actual_rs_total, impact_rs_total) %>%
  arrange(desc(impact_rs_total)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Runs Allowed Impact ──\n")
team_summary %>%
  select(Team, proj_ra_per_game, actual_ra_per_game, impact_ra_per_game,
         proj_ra_total, actual_ra_total, impact_ra_total) %>%
  arrange(impact_ra_total) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Run Differential Impact ──\n")
team_summary %>%
  select(Team, proj_run_diff, actual_run_diff, impact_run_diff) %>%
  arrange(desc(impact_run_diff)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Pythagorean Sanity Check ──\n")
team_summary %>%
  select(Team, proj_win_pct, pyth_proj_win_pct, pyth_vs_model_gap) %>%
  arrange(desc(abs(pyth_vs_model_gap))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n── Confidence & Prediction Intervals ──\n")
cat("95% CI on projected wins: proj_wins ± 1.96 * sqrt(sum(p*(1-p)))\n")
cat("95% CI on projected RS/G and RA/G: proj_mean ± 1.96 * RMSE / sqrt(n)\n")
cat("95% PI on single game RS/RA: pred ± 1.96 * RMSE = ±",
    round(Z95 * RMSE_RS, 2), "runs\n\n")

team_summary %>%
  select(Team, proj_wins, proj_wins_ci_lo, proj_wins_ci_hi,
         proj_rs_per_game, proj_rs_pg_ci_lo, proj_rs_pg_ci_hi,
         proj_ra_per_game, proj_ra_pg_ci_lo, proj_ra_pg_ci_hi,
         actual_wins, impact_wins) %>%
  arrange(desc(impact_wins)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)
cat("W/L model = classification model | RD method = proj RS > proj RA per game\n\n")

method_compare <- team_summary %>%
  select(Team, actual_wins, proj_wins, proj_wins_rd) %>%
  mutate(wl_error = actual_wins - proj_wins,
         rd_error = actual_wins - proj_wins_rd) %>%
  arrange(desc(actual_wins))

print(as.data.frame(method_compare), row.names = FALSE)

cat(sprintf("\n  W/L model  MAE: %.2f wins | RMSE: %.2f wins\n",
            mean(abs(method_compare$wl_error)),
            sqrt(mean(method_compare$wl_error^2))))
cat(sprintf("  RD method  MAE: %.2f wins | RMSE: %.2f wins\n",
            mean(abs(method_compare$rd_error)),
            sqrt(mean(method_compare$rd_error^2))))

# 8. SAVE
write.csv(pred_df,      "../outputs/predictions_2026_game_level.csv",  row.names = FALSE)
write.csv(team_summary, "../outputs/predictions_2026_team_summary.csv", row.names = FALSE)
