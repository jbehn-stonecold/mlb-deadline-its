library(dplyr)
library(ggplot2)
library(patchwork)

#  VISUAL 2 — GAME-BY-GAME ERROR SCATTER
#  Each dot = one game. Model absolute error on x-axis, baseline on y-axis.
#  Points BELOW diagonal = model won that game.
#  Points ABOVE diagonal = baseline won.

test25 <- read.csv("../outputs/02b_test_results.csv", stringsAsFactors = FALSE)
feats  <- read.csv("../data/model_features.csv",   stringsAsFactors = FALSE)

feats25 <- feats %>%
  filter(Season == 2025) %>%
  mutate(Date = as.character(Date)) %>%
  select(Team, Season, Date, cum_R_per_game, cum_RA_per_game)

# Add row index before merge to handle doubleheaders
# (same Team+Date appears twice on doubleheader days, causing duplicate rows)
test25_idx <- test25 %>% mutate(.row = row_number())

df <- test25_idx %>%
  mutate(Date = as.character(Date)) %>%
  left_join(feats25, by = c("Team", "Season", "Date")) %>%
  distinct(.row, .keep_all = TRUE) %>%  # remove duplicate rows from doubleheaders
  select(-.row) %>%
  mutate(
    ae_rs_model = abs(runs_scored  - pred_runs_scored),
    ae_ra_model = abs(runs_allowed - pred_runs_allowed),
    ae_rs_pyth  = abs(runs_scored  - cum_R_per_game),
    ae_ra_pyth  = abs(runs_allowed - cum_RA_per_game),
    rs_winner   = case_when(ae_rs_model < ae_rs_pyth ~ "Model better",
                             ae_rs_model > ae_rs_pyth ~ "Baseline better",
                             TRUE ~ "Tie"),
    ra_winner   = case_when(ae_ra_model < ae_ra_pyth ~ "Model better",
                             ae_ra_model > ae_ra_pyth ~ "Baseline better",
                             TRUE ~ "Tie")
  )

# Count wins
rs_model_wins <- sum(df$rs_winner == "Model better")
rs_pyth_wins  <- sum(df$rs_winner == "Baseline better")
ra_model_wins <- sum(df$ra_winner == "Model better")
ra_pyth_wins  <- sum(df$ra_winner == "Baseline better")

col_model    <- "#1D9E75"
col_baseline <- "#D85A30"
col_tie      <- "#B4B2A9"
col_grid     <- "#EEEEEE"

base_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "#444444", size = 10),
    axis.title       = element_text(color = "#333333", size = 11),
    plot.title       = element_text(color = "#111111", size = 13, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(color = "#666666", size = 10, hjust = 0.5, margin = margin(b=6)),
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    plot.margin      = margin(12, 14, 10, 12)
  )

make_scatter <- function(data, x_col, y_col, win_col, model_wins, pyth_wins, title) {
  lim <- max(data[[x_col]], data[[y_col]], na.rm = TRUE) * 1.05

  label <- sprintf("Model better: %d games (%.1f%%)\nBaseline better: %d games (%.1f%%)\nTies: %d games",
    model_wins, 100*model_wins/nrow(data),
    pyth_wins,  100*pyth_wins/nrow(data),
    nrow(data) - model_wins - pyth_wins)

  ggplot(data, aes_string(x = x_col, y = y_col, color = win_col)) +
    geom_abline(slope = 1, intercept = 0, color = "#555555",
                linewidth = 0.8, linetype = "dashed") +
    geom_point(alpha = 0.35, size = 1.2) +
    annotate("text", x = lim * 0.02, y = lim * 0.97,
             label = "Baseline\nbetter", color = col_baseline,
             size = 3.5, hjust = 0, vjust = 1, fontface = "bold") +
    annotate("text", x = lim * 0.97, y = lim * 0.03,
             label = "Model\nbetter", color = col_model,
             size = 3.5, hjust = 1, vjust = 0, fontface = "bold") +
    annotate("label", x = lim * 0.97, y = lim * 0.97, label = label,
             hjust = 1, vjust = 1, size = 3.5, color = "#222222",
             fill = "#F4F9F4", label.size = 0.4, label.padding = unit(0.45, "lines"),
             lineheight = 1.5) +
    scale_color_manual(
      values = c("Model better" = col_model,
                 "Baseline better" = col_baseline,
                 "Tie" = col_tie),
      name = NULL
    ) +
    scale_x_continuous(limits = c(0, lim)) +
    scale_y_continuous(limits = c(0, lim)) +
    coord_fixed() +
    labs(title    = title,
         subtitle = "Points below diagonal = model had lower error on that game",
         x = "Pythagorean baseline absolute error (runs)",
         y = "CatBoost model absolute error (runs)") +
    base_theme
}

p_rs <- make_scatter(df, "ae_rs_pyth", "ae_rs_model", "rs_winner",
                     rs_model_wins, rs_pyth_wins,
                     "Runs Scored — Per-Game Absolute Error")

p_ra <- make_scatter(df, "ae_ra_pyth", "ae_ra_model", "ra_winner",
                     ra_model_wins, ra_pyth_wins,
                     "Runs Allowed — Per-Game Absolute Error")

final <- (p_rs | p_ra) +
  plot_annotation(
    title    = "CatBoost Model vs Pythagorean Baseline — Game-by-Game Error Comparison",
    subtitle = sprintf("2025 Held-Out Test Set  |  n = %d games  |  Each point = one game", nrow(df)),
    caption  = "Teal = model had lower error  |  Orange = baseline had lower error  |  Gray = tie\nPoints below the dashed diagonal line indicate games where the CatBoost model outperformed the baseline."
  )

ggsave("../model-validation/game_scatter.png", final,
       width = 14, height = 7, dpi = 200, bg = "white")