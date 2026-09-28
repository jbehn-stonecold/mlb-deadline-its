# MLB Game Outcome Prediction

An R / CatBoost pipeline that predicts MLB game and season outcomes (runs scored, runs allowed, win/loss) from team and pitcher game logs. Built as an ECON 3060 project, using three gradient-boosted CatBoost models trained on Stathead/Sports-Reference historical data.

The repo is split by season:

- **[`2025/`](2025/)** — the completed analysis: full pipeline (scrape → feature engineering → training/tuning → prediction → validation), final results, and the class presentation.
- **[`2026/`](2026/)** — the second completed season: the pipeline rebuilt with a more disciplined numbering scheme and additional validation steps, used to track 2026 live and then scored against actual second-half results.

## Method (applies to both seasons)

1. Scrape team batting/pitching game logs and individual pitcher game logs from Stathead/Sports-Reference.
2. Engineer rolling and cumulative features (form, run differential, bullpen/starter splits, etc.) from game logs prior to each game.
3. Train three CatBoost models — runs scored, runs allowed, win/loss — with grid search and SHAP-based feature selection.
4. Predict remaining/held-out games and compare against a Pythagorean-expectation baseline.
5. Visualize team-level and league-level results (interrupted time-series panels, confusion matrix, RMSE by team, playoff/standings projections).

## Running it locally

Each season's scripts are numbered to run in order and assume your R working directory is set to that season's own `scripts/` folder (e.g. in RStudio: *Session → Set Working Directory → To Source File Location*).

Raw scraped data, trained models, and generated outputs are not committed (see `.gitignore`) — running the scripts from `00_...R` onward regenerates them into sibling `data/`, `models/`, and `outputs/` folders.

Scraping requires a Sports-Reference/Stathead login. Copy `.env.example` to `.env`, fill in your credentials, and load it before running (e.g. `readRenviron(".env")` at the top of your R session, or the `dotenv` package).

See each season's own README for specifics.
