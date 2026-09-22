# 2025 Season — Completed Analysis

The original ECON 3060 project: three CatBoost models (runs scored, runs allowed, win/loss) trained on 2005–2024 team and pitcher game logs, used to project the rest of the 2025 season from the trade deadline forward and evaluate the projection against what actually happened.

This is the finished pipeline — the scripts here are the final version of each step, with earlier drafts and scratch work left out. See the [top-level README](../README.md) for the overall method.

## Pipeline (run in order)

| Script | Purpose |
|---|---|
| `00_scrape_pitcher_games.R` | Scrape individual pitcher game logs from Stathead |
| `01_prepare_and_engineer.R` | Build rolling/cumulative features from raw game logs |
| `02_train_models.R` | Initial CatBoost training pass |
| `02b_shap_feature_selection.R` | SHAP-based feature pruning |
| `02c_extensive_optimization.R` | Grid search + SHAP ranking + threshold tuning |
| `02d_direct_retrain.R` | Retrain all three models on the selected features |
| `02e_retrain_wl_only.R` | Targeted retrain of the win/loss model |
| `03_predict_2025.R` | Generate game- and team-level predictions |
| `04_its_visualization.R` | Per-team interrupted-time-series panels (pre/post trade deadline) |
| `05_team_impact_summary.R` | League-wide deadline impact summary, with confidence intervals |
| `08_shap_importance.R` | Final SHAP importance plot |
| `10_rmse_diagnostics.R` | RMSE diagnostics across models/teams |

Scripts assume your R working directory is this `scripts/` folder, and read/write sibling `../data/`, `../models/`, and `../outputs/` folders (not committed — see the top-level `.gitignore`). Re-run `00`–`01` to regenerate the raw/engineered data these scripts expect.

## Outputs

`outputs/` holds the final numeric results: retrain test results, held-out test predictions, and hyperparameter tuning results for each of the three models.

## Visuals

`visuals/` holds the final charts and the class presentation (`Final_Presentation_ECON_3060.pptx`): baseline comparison, deadline impact summary, two example team ITS panels (DET, NYM), confusion matrix, game-level scatter, team RMSE, and team prediction accuracy.
