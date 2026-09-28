library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)

#  PUBLISHED DEADLINE STANDINGS vs ACTUAL 2026 FINAL STANDINGS
#
#  Scores the projection exactly as published in
#  ../predictions/2026_overall_standings_projection.png: each team's
#  projected full-season record and playoff status (same rules as 15/16)
#  next to its actual final record and playoff status.
#
#  Input:  ../outputs/schedule_predictions_2026_team_summary.csv (as published)
#          ../data/stathead_team.csv (actual 2026 results)
#  Output: ../model-validation/2026_published_standings_vs_actual.png

OUT_DIR   <- "../outputs/"
VAL_DIR   <- "../model-validation/"

# Real MLB division/league alignment (same as 13/15/16)
divisions <- tribble(
  ~Team, ~League, ~Division,
  "NYY","AL","East",  "BOS","AL","East",  "TBR","AL","East",  "TOR","AL","East",  "BAL","AL","East",
  "CLE","AL","Central","MIN","AL","Central","DET","AL","Central","CHW","AL","Central","KCR","AL","Central",
  "HOU","AL","West",  "SEA","AL","West",  "TEX","AL","West",  "LAA","AL","West",  "OAK","AL","West",
  "ATL","NL","East",  "PHI","NL","East",  "NYM","NL","East",  "MIA","NL","East",  "WSN","NL","East",
  "MIL","NL","Central","CHC","NL","Central","STL","NL","Central","CIN","NL","Central","PIT","NL","Central",
  "LAD","NL","West",  "SDP","NL","West",  "ARI","NL","West",  "SFG","NL","West",  "COL","NL","West"
)

# Playoff status from a wins column — same rules as 15/16
# (best record in division; top 3 non-division-winners per league)
playoff_status <- function(d, wins_col) {
  d %>%
    group_by(League, Division) %>%
    mutate(div_rank = rank(-.data[[wins_col]], ties.method = "first")) %>%
    group_by(League) %>%
    mutate(wc_rank = rank(ifelse(div_rank == 1, Inf, -.data[[wins_col]]), ties.method = "first")) %>%
    ungroup() %>%
    mutate(status = case_when(div_rank == 1 ~ "Division Winner",
                              wc_rank <= 3  ~ "Wild Card",
                              TRUE          ~ "Missed Playoffs")) %>%
    pull(status)
}

# 1. PUBLISHED PROJECTION
proj <- read.csv(paste0(OUT_DIR, "schedule_predictions_2026_team_summary.csv"),
                 stringsAsFactors = FALSE) %>%
  transmute(Team,
            proj_w = round(proj_full_season_wins),
            proj_l = round(proj_full_season_losses)) %>%
  inner_join(divisions, by = "Team")
proj$proj_status <- playoff_status(proj, "proj_w")

# 2. ACTUAL FINAL STANDINGS
raw <- read.csv("../data/stathead_team.csv", stringsAsFactors = FALSE) %>%
  mutate(Team = recode(Team, "FLA" = "MIA", "TBD" = "TBR", "ATH" = "OAK"),
         win  = as.integer(substr(Result, 1, 1) == "W"))

final <- raw %>%
  group_by(Team) %>%
  summarise(act_g = n(), act_w = sum(win), .groups = "drop") %>%
  mutate(act_l = act_g - act_w) %>%
  inner_join(divisions, by = "Team")
final$act_status <- playoff_status(final, "act_w")

# Flag any tie at a playoff cutoff — the rank rule above can't settle those
# (real tiebreaker is head-to-head)
cut_ties <- final %>%
  group_by(League, Division) %>% filter(sum(act_w == max(act_w)) > 1, act_w == max(act_w)) %>% ungroup()
if (nrow(cut_ties) > 0) {
  cat("WARNING: tie for a division lead in actual standings — check head-to-head:\n")
  print(cut_ties %>% select(Team, League, Division, act_w))
}

df <- proj %>%
  inner_join(final %>% select(Team, act_w, act_l, act_status), by = "Team") %>%
  mutate(diff         = act_w - proj_w,
         status_right = proj_status == act_status,
         made_right   = (proj_status != "Missed Playoffs") == (act_status != "Missed Playoffs"))

mae       <- mean(abs(df$diff))
exact_n   <- sum(df$diff == 0)
within3_n <- sum(abs(df$diff) <= 3)
field_n   <- sum(df$proj_status != "Missed Playoffs" & df$act_status != "Missed Playoffs")
div_n     <- sum(df$proj_status == "Division Winner" & df$act_status == "Division Winner")

cat(sprintf("Full-season wins MAE: %.2f | exact: %d | within 3: %d / 30\n", mae, exact_n, within3_n))
cat(sprintf("Playoff teams correctly called: %d / 12 | division winners: %d / 6 | status exactly right: %d / 30\n",
            field_n, div_n, sum(df$status_right)))
write.csv(df, paste0(OUT_DIR, "published_standings_vs_actual_2026.csv"), row.names = FALSE)

# 3. TABLE CHART — same look as 15_overall_standings_projection
col_winner <- "#1D9E75"
col_wc     <- "#378ADD"
col_missed <- "#B4B2A9"
col_miss   <- "#D85A30"
col_text   <- "#222222"
col_muted  <- "#888888"
col_grid   <- "#DDDDDD"

status_color <- function(status) case_when(
  status == "Division Winner" ~ col_winner,
  status == "Wild Card"       ~ col_wc,
  TRUE                        ~ col_missed
)

col_x <- c(team = 0, proj = 1.1, act = 2.3, diff = 3.5, pstat = 4.5, astat = 6.1)
x_max <- 7.8

make_division_table <- function(div_data, title_str) {
  div_data <- div_data %>%
    arrange(desc(act_w)) %>%
    mutate(
      row_y      = rev(seq_len(n())),
      act_col    = status_color(act_status),
      proj_col   = status_color(proj_status),
      diff_lab   = ifelse(diff > 0, paste0("+", diff), as.character(diff)),
      diff_col   = ifelse(abs(diff) >= 5, col_miss, col_text),
      astat_lab  = ifelse(status_right, act_status, paste0(act_status, "  ✗"))
    )
  y_top <- nrow(div_data) + 1

  ggplot(div_data) +
    # row tint = ACTUAL outcome
    geom_rect(aes(xmin = -0.2, xmax = x_max, ymin = row_y - 0.45, ymax = row_y + 0.45,
                  fill = act_col), alpha = 0.15) +
    scale_fill_identity() +
    annotate("text", x = col_x, y = y_top, hjust = 0,
             label = c("Team", "Projected", "Actual", "Diff", "Projected status", "Actual status"),
             fontface = "bold", size = 3.0, color = col_text) +
    geom_hline(yintercept = y_top - 0.5, color = col_grid, linewidth = 0.5) +
    geom_text(aes(x = col_x["team"], y = row_y, label = Team),
              hjust = 0, size = 3.2, fontface = "bold", color = col_text) +
    geom_text(aes(x = col_x["proj"], y = row_y, label = sprintf("%d-%d", proj_w, proj_l)),
              hjust = 0, size = 3.0, color = col_muted) +
    geom_text(aes(x = col_x["act"], y = row_y, label = sprintf("%d-%d", act_w, act_l)),
              hjust = 0, size = 3.0, fontface = "bold", color = col_text) +
    geom_text(aes(x = col_x["diff"], y = row_y, label = diff_lab, color = diff_col),
              hjust = 0, size = 3.0, fontface = "bold") +
    geom_text(aes(x = col_x["pstat"], y = row_y, label = proj_status, color = proj_col),
              hjust = 0, size = 2.7) +
    geom_text(aes(x = col_x["astat"], y = row_y, label = astat_lab, color = act_col),
              hjust = 0, size = 2.7, fontface = "bold") +
    scale_color_identity() +
    scale_x_continuous(limits = c(-0.2, x_max), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.4, y_top + 0.6), expand = c(0, 0)) +
    labs(title = title_str) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 11.5, color = "#111111",
                                     margin = margin(b = 4)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(6, 10, 6, 10)
    )
}

div_tbl <- function(lg, dv) make_division_table(df %>% filter(League == lg, Division == dv),
                                                paste(lg, dv))

p_al <- div_tbl("AL", "East") / div_tbl("AL", "Central") / div_tbl("AL", "West")
p_nl <- div_tbl("NL", "East") / div_tbl("NL", "Central") / div_tbl("NL", "West")

legend_text <- paste0(
  "Projected = full-season W-L as published at the Aug 3 deadline (2026_overall_standings_projection.png)\n",
  "Actual = final 2026 W-L  |  Diff = actual wins minus projected wins (red = off by 5+)\n\n",
  "Row color = actual outcome:  Division Winner (green)  |  Wild Card (blue)  |  Missed Playoffs (gray)\n",
  "✗ = projected playoff status did not match the actual outcome\n\n",
  sprintf("Summary:  average miss %.1f wins  |  %d of 30 within 3 wins  |  %d exact  |  ",
          mae, within3_n, exact_n),
  sprintf("%d of 12 playoff teams called  |  %d of 6 division winners called", field_n, div_n)
)

p_legend <- ggplot() +
  annotate("text", x = 0.02, y = 0.5, label = legend_text, hjust = 0, vjust = 0.5,
           size = 3.0, color = "#333333", lineheight = 1.4) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F8F8F8", color = "#DDDDDD", linewidth = 0.5),
        plot.margin = margin(8, 8, 8, 8)) +
  xlim(0, 1) + ylim(0, 1)

final_plot <- (p_al | p_nl) / p_legend +
  plot_layout(heights = c(10, 2)) +
  plot_annotation(
    title    = "2026 Deadline Projection vs Final Standings",
    subtitle = sprintf("Published Aug 3 projection scored against how the season finished  |  %d of 12 playoff teams called  |  average miss %.1f wins",
                       field_n, mae),
    caption  = "Playoff status uses the same rules as the published chart: best record in division; top 3 non-division-winners per league."
  )

out_file <- paste0(VAL_DIR, "2026_published_standings_vs_actual.png")
ggsave(out_file, final_plot, width = 16, height = 16.5, dpi = 200, bg = "white")
cat(sprintf("Saved %s\n", out_file))
