# 2026 Season — In Progress

The same modeling approach as [`2025/`](../2025/), rebuilt with a cleaner numbered pipeline and applied to the 2026 season. Post-trade-deadline predictions for the rest of the season are complete; second-half validation (scripts `05`–`07`) is still pending until enough second-half games have been played to score against.

## Status

- **Done:** scraping, feature engineering, model training/tuning, model validation against 2025 as a baseline, and post-deadline predictions for every team's remaining schedule.
- **Pending:** re-running `05_team_impact_summary.R`, `06_its_visualization.R`, and `07_team_prediction_accuracy.R` once second-half 2026 results are in, to validate the post-deadline projections the same way `2025/` was validated.

## Pipeline (run in order)

| Script | Purpose |
|---|---|
| `00_scrape_2026_schedule.R`, `00_scrape_ind_pitcher_games.R`, `00_scrape_team_hitter_games.R`, `00_scrape_team_pitcher_games.R` | Scrape schedule and game logs from Baseball-Reference/Stathead |
| `00_merge_batting_pitching.R` | Merge batting and pitching game logs |
| `01_prepare_and_engineer.R` | Build rolling/cumulative features |
| `02a_grid_search.R` | Hyperparameter grid search + SHAP feature selection |
| `02b_direct_retrain.R` | Retrain on selected features |
| `02c_final_retrain_all_data.R` | Final retrain on all available data |
| `03_predict_2026.R` | Generate game- and team-level predictions |
| `04_2025_baseline_comparison.R`, `08_confusion_matrix.R`, `09_game_scatter.R`, `10_team_rmse.R` | Model validation against the 2025 baseline |
| `05_team_impact_summary.R`, `06_its_visualization.R`, `07_team_prediction_accuracy.R` | Post-deadline validation — **pending second-half 2026 data** |
| `11_build_and_predict_2026_schedule.R`, `11b_add_missing_games_to_schedule.R`, `11b_patch_missing_games.R` | Build the remaining-schedule prediction set |
| `12_team_projection_panels.R` | Per-team rest-of-season projection panels |
| `13_playoff_projections.R` | Per-team playoff-odds panels |
| `14_win_pct_comparison.R` | Per-team win-percentage comparison panels |
| `15_overall_standings_projection.R`, `16_division_winners_and_wildcard_race.R` | League-wide standings and division/wild-card projections |

Scripts assume your R working directory is this `scripts/` folder, and read/write sibling `../data/`, `../models/`, and `../outputs/` folders (not committed — see the top-level `.gitignore`). `12_team_projection_panels.R` and `06_its_visualization.R` also expect a manually-maintained `../data/trades_2026.xlsx` trade-deadline tracker, which isn't produced by any script — supply your own.

## Models

`models/` holds the trained CatBoost models (`.cbm`) and their selected-feature lists for each of the three targets.

## Outputs

`outputs/` holds prediction CSVs (game- and team-level, for both the 2025 baseline check and the 2026 season) and hyperparameter/SHAP tuning results.

## Model validation

`model-validation/` holds the 2025-baseline validation charts (confusion matrix, game-level scatter, team RMSE). Post-deadline validation charts will land here once second-half data is available.

## Predictions

`predictions/` holds the post-deadline projections: overall standings and division/wild-card race charts, plus one panel per team in `team-projections/`, `playoff-projections/`, and `win-percentage-comparison/`.
