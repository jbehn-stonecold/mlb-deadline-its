#  2026 END-OF-SEASON VALIDATION
#
#  Re-runs the 2025-baseline validation scripts (04, 08, 09, 10) against the
#  2026 second half, and renders 06_its_visualization.R for every team.
#  The source scripts are not modified — each is read in, re-pointed at the
#  2026 inputs/outputs via text substitution, and evaluated.
#
#  Run after 03_predict_2026.R (needs predictions_2026_game_level.csv and
#  predictions_2026_team_summary.csv) and 01_prepare_and_engineer.R
#  (needs model_features.csv with 2026 rows).
#
#  Outputs (all in ../model-validation/):
#    2026_baseline_comparison.png, 2026_confusion_matrix.png,
#    2026_game_scatter.png, 2026_team_rmse.png
#    its-panels/<TEAM>_2026_its_panel.png  (one per team)
#
#  Re-run once ../data/trades_2026.xlsx exists to fill in each ITS panel's
#  deadline-moves box (until then it shows a placeholder).

run_substituted <- function(script, subs) {
  code <- readLines(script, warn = FALSE, encoding = "UTF-8")
  for (k in names(subs)) code <- gsub(k, subs[[k]], code, fixed = TRUE)
  eval(parse(text = code, encoding = "UTF-8"), envir = new.env(parent = globalenv()))
}

# 1. BASELINE COMPARISON / CONFUSION MATRIX / SCATTER / TEAM RMSE
# predictions_2026_game_level.csv also carries every feature column, so trim it
# to the same columns as 02b_test_results.csv — otherwise the scripts' own join
# onto model_features.csv produces .x/.y duplicates.
eval_subs <- c(
  'read.csv("../outputs/02b_test_results.csv", stringsAsFactors = FALSE)' =
    'read.csv("../outputs/predictions_2026_game_level.csv", stringsAsFactors = FALSE)[, names(read.csv("../outputs/02b_test_results.csv", nrows = 1))]',
  "Season == 2025"         = "Season == 2026",
  "2025 Held-Out Test Set" = "2026 Second Half (post-deadline, out-of-sample)",
  "2025 held-out test set" = "2026 second half",
  "2025 Model Performance" = "2026 Model Performance",
  "model-validation/2025_baseline_comparison.png" = "model-validation/2026_baseline_comparison.png",
  "model-validation/confusion_matrix.png" = "model-validation/2026_confusion_matrix.png",
  "model-validation/game_scatter.png"     = "model-validation/2026_game_scatter.png",
  "model-validation/team_rmse.png"        = "model-validation/2026_team_rmse.png"
)

for (s in c("04_2025_baseline_comparison.R", "08_confusion_matrix.R",
            "09_game_scatter.R", "10_team_rmse.R")) {
  cat(sprintf("\n── %s (2026) ──\n", s))
  run_substituted(s, eval_subs)
}

# 2. ITS PANELS FOR EVERY TEAM
dir.create("../model-validation/its-panels", showWarnings = FALSE)
teams <- sort(unique(read.csv("../outputs/predictions_2026_team_summary.csv")$Team))

for (t in teams) {
  res <- try(run_substituted("06_its_visualization.R", c(
    'TEAM_NAME   <- "NYM"' = sprintf('TEAM_NAME   <- "%s"', t),
    '"../model-validation/%s_2026_its_panel_FINAL.png"' =
      '"../model-validation/its-panels/%s_2026_its_panel.png"'
  )))
  cat(t, if (inherits(res, "try-error")) "FAILED" else "ok", "\n")
}
