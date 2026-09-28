# 2026 Season — Completed Analysis

The same modeling approach as [`2025/`](../2025/), rebuilt with a cleaner numbered pipeline and applied to the 2026 season. The regular season is over and the projections made at the Aug 3 trade deadline have been scored against what actually happened, the same way `2025/` was validated.

## Status

- **Done:** scraping, feature engineering, model training/tuning, post-deadline projections (made Aug 3), and end-of-season validation of those projections.
- **Pending:** the trade-deadline tracker `data/trades_2026.xlsx` hasn't been added yet. The per-team ITS panels in `model-validation/its-panels/` currently show a placeholder in their "deadline moves" box. Add the tracker and re-run `17_2026_season_validation.R` to fill them in. `12_team_projection_panels.R` also reads it.

## 2026 results

Everything below scores the per-game predictions saved at the deadline (`outputs/schedule_predictions_2026_game_level.csv`). The models were not re-run. The second half is the 739 games after Aug 3 (48–51 per team).

**Published standings vs final standings** (`18`, `model-validation/2026_published_standings_vs_actual.png`):

- 11 of 12 playoff teams called, and 5 of 6 division winners. CLE and CHW swapped AL Central division winner and wild card; SDP made it instead of ARI.
- Average miss of 2.8 full-season wins: 19 of 30 teams within 3 wins, and 3 exact (ARI, TEX, CIN).

**Second half vs 2025:**

| Metric | 2025 | 2026 |
|---|---|---|
| Team win projection MAE | 3.27 wins | **3.00 wins** |
| Team win projection RMSE | 3.91 wins | **3.54 wins** |
| Avg % of games off (`07`) | 6.2% | **6.1%** |
| Teams whose actual wins fell inside the 90% CI | 26 / 30 | **28 / 30** |
| Teams outside the 99% CI (wins) | 0 | 0 |
| Correlation, projected vs actual wins | 0.62 | **0.66** |
| Game-level W/L accuracy | 55.9% | **57.2%** |
| Game-level RS / RA RMSE | 3.20 / 3.19 | **3.09 / 3.09** |

Against the Pythagorean baseline, the model is significantly better on runs scored (paired t-test p < 0.001) and on win/loss calls (57.2% vs 53.2%, McNemar p = 0.005). Runs allowed is not significantly different (p = 0.36).

Biggest misses: **SDP** (+8 wins over projection), **COL** (−7), then MIL and NYM (+5) and DET and SFG (−5). **SEA** is the only team outside the 99% CI on any metric: it allowed about 1.35 more runs per game than projected.

## Known issues

- **Aug 3 games missing from the deadline projection.** The deadline scrape ran before that day's 8 games were final. They were left out of the first half, and the remaining schedule started Aug 4. `11b_add_missing_games_to_schedule.R` / `11b_patch_missing_games.R` then treated them as unscheduled makeup games. They added 8 placeholder games on Sep 28 and gave each a full win for the team with the higher projection. This moves a published full-season record by at most one win. `03` scores only games actually played, so the placeholders are ignored there. A team's ITS panel projection can therefore differ from its published record by one win (e.g. SDP: 83 vs 84).
- **`is_home` is always 0 in `model_features.csv`.** `01_prepare_and_engineer.R` tests `is.na(home_away)`, but `read.csv` reads the blank home marker as `""`, not `NA`. This affects the 2005–2025 training data too, so the models never learned home field. Fix it before the next retrain.

## Pipeline (run in order)

| Script | Purpose |
|---|---|
| `00_scrape_2026_schedule.R`, `00_scrape_ind_pitcher_games.R`, `00_scrape_team_hitter_games.R`, `00_scrape_team_pitcher_games.R` | Scrape schedule and game logs from Baseball-Reference/Stathead |
| `00_merge_batting_pitching.R` | Merge batting and pitching game logs |
| `01_prepare_and_engineer.R` | Build rolling/cumulative features |
| `02a_grid_search.R` | Hyperparameter grid search + SHAP feature selection |
| `02b_direct_retrain.R` | Retrain on selected features |
| `02c_final_retrain_all_data.R` | Final retrain on all available data |
| `03_predict_2026.R` | Score the saved deadline predictions (from `11`) against actual 2026 second-half results |
| `04_2025_baseline_comparison.R`, `08_confusion_matrix.R`, `09_game_scatter.R`, `10_team_rmse.R` | Model validation against the 2025 held-out season |
| `05_team_impact_summary.R`, `07_team_prediction_accuracy.R` | 2026 post-deadline validation: deadline impact summary and team win-prediction accuracy |
| `06_its_visualization.R` | Single-team ITS panel (pre/post deadline) |
| `11_build_and_predict_2026_schedule.R`, `11b_add_missing_games_to_schedule.R`, `11b_patch_missing_games.R` | Build the remaining-schedule prediction set (made mid-season, before results were in) |
| `12_team_projection_panels.R` | Per-team rest-of-season projection panels |
| `13_playoff_projections.R` | Per-team playoff-odds panels |
| `14_win_pct_comparison.R` | Per-team win-percentage comparison panels |
| `15_overall_standings_projection.R`, `16_division_winners_and_wildcard_race.R` | League-wide standings and division/wild-card projections |
| `17_2026_season_validation.R` | Re-runs `04`/`08`/`09`/`10` against the 2026 second half, and `06` for all 30 teams |
| `18_published_standings_vs_actual.R` | Published deadline standings (as in `predictions/`) vs actual final standings and playoff field |

Scripts assume your R working directory is this `scripts/` folder, and read/write sibling `../data/`, `../models/`, and `../outputs/` folders (`data/` is not committed; see the top-level `.gitignore`). `12_team_projection_panels.R` and `06_its_visualization.R` also read a manually maintained `../data/trades_2026.xlsx` trade-deadline tracker, which no script produces. `06` falls back to a placeholder if it's missing.

To score a finished season you only need that season's data: features are computed within each team-season and the models are already trained. Run the `00_scrape_*` scripts for 2026 only (the team scrapers loop over `2005:2026`, so narrow that range), then `00_merge`, `01`, `03`, `05`, `07`, `17` and `18`.

## Models

`models/` holds the trained CatBoost models (`.cbm`) and their selected-feature lists for each of the three targets.

## Outputs

`outputs/` holds prediction CSVs (game- and team-level, for the 2025 held-out check, the mid-season schedule projection, and the scored 2026 second half in `predictions_2026_*.csv`) plus hyperparameter/SHAP tuning results.

## Model validation

`model-validation/` holds:

- **2025 held-out validation:** `2025_baseline_comparison.png`, `confusion_matrix.png`, `game_scatter.png`, `team_rmse.png`
- **2026 season validation:** `2026_deadline_impact_summary.png`, `team_prediction_accuracy.png`, `2026_baseline_comparison.png`, `2026_confusion_matrix.png`, `2026_game_scatter.png`, `2026_team_rmse.png`, `2026_published_standings_vs_actual.png`, and one ITS panel per team in `its-panels/`

## Predictions

`predictions/` holds the mid-season post-deadline projections: overall standings and division/wild-card race charts, plus one panel per team in `team-projections/`, `playoff-projections/`, and `win-percentage-comparison/`.
