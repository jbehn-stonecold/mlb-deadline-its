#  README PANELS — TOP OVER/UNDERPERFORMER, 2025 AND 2026
#
#  Renders the four team panels shown side by side in the top-level README,
#  all with the same layout (2026/scripts/06_its_visualization.R), so 2025
#  and 2026 line up. 06 itself isn't modified — its code is read in and
#  re-pointed at each season's inputs via text substitution.
#
#  Inputs (data/ folders are gitignored — see each season's README):
#    2025: 2025/data/stathead_team.csv, 2025/data/predictions_2025_game_level.csv
#          (the original 2025 deadline projection), 2025/data/trades_2025.xlsx
#    2026: 2026/data/stathead_team.csv, 2026/outputs/predictions_2026_game_level.csv,
#          2026/data/trades_2026.xlsx
#
#  Run with the working directory set to this readme-visuals/ folder.

ROOT <- normalizePath("..", winslash = "/")
OUT  <- normalizePath(".", winslash = "/")

panels <- list(
  list(team = "ATL", year = 2025),   # top overperformer 2025  (+6)
  list(team = "NYM", year = 2025),   # top underperformer 2025 (-8)
  list(team = "SDP", year = 2026),   # top overperformer 2026  (+8)
  list(team = "COL", year = 2026)    # top underperformer 2026 (-7)
)

season_subs <- function(year) {
  if (year == 2026) {
    c('"../data/trades_2026.xlsx"'                   = sprintf('"%s/2026/data/trades_2026.xlsx"', ROOT),
      '"../data/stathead_team.csv"'                  = sprintf('"%s/2026/data/stathead_team.csv"', ROOT),
      '"../outputs/predictions_2026_game_level.csv"' = sprintf('"%s/2026/outputs/predictions_2026_game_level.csv"', ROOT))
  } else {
    c('"../data/trades_2026.xlsx"'                   = sprintf('"%s/2025/data/trades_2025.xlsx"', ROOT),
      '"../data/stathead_team.csv"'                  = sprintf('"%s/2025/data/stathead_team.csv"', ROOT),
      '"../outputs/predictions_2026_game_level.csv"' = sprintf('"%s/2025/data/predictions_2025_game_level.csv"', ROOT),
      'Season == 2026, Date <= as.Date("2026-08-03")' = 'Season == 2025, Date <= as.Date("2025-07-31")',
      "2026 Full Season"                             = "2025 Full Season",
      "(August 3, 2026)"                             = "(July 31, 2025)",
      "at the 2026 deadline"                         = "at the 2025 deadline",
      "Deadline line marks August 3."                = "Deadline line marks July 31.")
  }
}

src <- readLines(file.path(ROOT, "2026/scripts/06_its_visualization.R"), warn = FALSE, encoding = "UTF-8")

for (p in panels) {
  subs <- c(season_subs(p$year),
            'TEAM_NAME   <- "NYM"' = sprintf('TEAM_NAME   <- "%s"', p$team),
            '"../model-validation/%s_2026_its_panel_FINAL.png"' =
              sprintf('"%s/%%s_%d_its_panel.png"', OUT, p$year))
  code <- src
  for (k in names(subs)) code <- gsub(k, subs[[k]], code, fixed = TRUE)
  eval(parse(text = code, encoding = "UTF-8"), envir = new.env(parent = globalenv()))
  cat(sprintf("Saved %s_%d_its_panel.png\n", p$team, p$year))
}
