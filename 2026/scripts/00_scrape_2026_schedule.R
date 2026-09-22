library(httr)
library(rvest)
library(dplyr)

#  2026 REMAINING-SCHEDULE SCRAPER
#
#  The other 00_* scrapers only pull games that already have a final score
#  (Stathead's game finder only indexes completed games), so they can never
#  tell us who a team plays after the trade deadline until those games are
#  already over. This script instead reads each team's public schedule page
#  on baseball-reference.com, which lists every game for the season --
#  played and upcoming -- with opponent and home/away, but no score for
#  games not yet played.
#
#  Output: one row per team per remaining (post-July-31) 2026 game, with
#  Team, Opp, Date, is_home. This feeds 11_build_and_predict_2026_schedule,
#  which attaches pre-deadline features and runs the trained CatBoost
#  models on these rows to get real per-game predictions for the rest of
#  the season.
#
#  NOTE: table id/column names below (#team_schedule, "Opp", the blank
#  home/away column) reflect Baseball-Reference's standard schedule-page
#  layout. If the site's markup has changed, re-check the saved
#  debug_schedule_<TEAM>.html for the actual table id/column names and
#  adjust the two lines marked below.

# Set STATHEAD_USERNAME / STATHEAD_PASSWORD in a local .env (see .env.example)
USERNAME <- Sys.getenv("STATHEAD_USERNAME")
PASSWORD <- Sys.getenv("STATHEAD_PASSWORD")

# Login (schedule pages are public, but logging in keeps a consistent
# session/cookie set and matches how the rest of this pipeline scrapes)
h <- handle("https://www.baseball-reference.com")
login_response <- POST(
  url    = "https://www.baseball-reference.com/users/login.cgi",
  handle = h,
  body   = list(
    username = USERNAME,
    password = PASSWORD,
    login    = "Login"
  ),
  encode = "form",
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    `Referer`    = "https://www.baseball-reference.com/users/login.cgi"
  )
)
cat("Login status:", status_code(login_response), "\n")

# Baseball-Reference's current team URL codes. The Athletics currently
# scrape under "ATH" (post-relocation) -- recoded to "OAK" below to match
# the franchise unification used everywhere else in this pipeline.
TEAM_CODES <- c("ARI","ATL","BAL","BOS","CHC","CHW","CIN","CLE","COL","DET",
                 "HOU","KCR","LAA","LAD","MIA","MIL","MIN","NYM","NYY","ATH",
                 "PHI","PIT","SDP","SEA","SFG","STL","TBR","TEX","TOR","WSN")

SEASON   <- 2026
DEADLINE <- as.Date(sprintf("%d-08-03", SEASON))  # 2026 trade deadline moved to Aug 3

all_teams <- list()

for (code in TEAM_CODES) {
  cat(sprintf("\n========== %s ==========\n", code))

  url <- sprintf("https://www.baseball-reference.com/teams/%s/%d-schedule-scores.shtml",
                 code, SEASON)

  resp <- GET(
    url,
    handle = h,
    add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      `Referer`    = "https://www.baseball-reference.com/"
    )
  )

  cat("  HTTP status:", status_code(resp), "\n")

  if (status_code(resp) != 200) {
    cat("  Non-200 response — saving debug and skipping.\n")
    writeLines(content(resp, "text", encoding = "UTF-8"),
               sprintf("debug_schedule_%s.html", code))
    next
  }

  page_html <- read_html(content(resp, "text", encoding = "UTF-8"))

  # <-- adjust this selector if the schedule table id has changed
  table_node <- html_element(page_html, "#team_schedule")

  if (is.na(table_node) || is.null(table_node)) {
    cat("  No schedule table found (#team_schedule) — saving debug and skipping.\n")
    writeLines(content(resp, "text", encoding = "UTF-8"),
               sprintf("debug_schedule_%s.html", code))
    next
  }

  raw_tbl <- html_table(table_node, header = TRUE, fill = TRUE)

  # Sanitize column names before any dplyr verb touches this data frame --
  # dplyr::transmute() refuses to run on a data frame with ANY NA/blank
  # column name, even ones not directly referenced. Baseball-Reference's
  # schedule table has a genuinely blank header for the home/away column,
  # which html_table() carries through as NA or "" rather than the
  # positional "...N" naming seen elsewhere.
  names(raw_tbl) <- make.names(names(raw_tbl), unique = TRUE)

  if (!("Opp" %in% names(raw_tbl)) || !("Date" %in% names(raw_tbl))) {
    cat("  Expected columns (Date, Opp) not found — saving debug and skipping.\n")
    writeLines(content(resp, "text", encoding = "UTF-8"),
               sprintf("debug_schedule_%s.html", code))
    next
  }

  # <-- adjust here if Baseball-Reference renames the blank home/away column
  # It sits immediately to the left of "Opp": blank = home game, "@" = away.
  opp_idx       <- which(names(raw_tbl) == "Opp")[1]
  home_away_col <- names(raw_tbl)[opp_idx - 1]

  df <- raw_tbl %>%
    transmute(
      Team        = code,
      Date_raw    = as.character(Date),
      Opp         = as.character(Opp),
      home_marker = trimws(as.character(.data[[home_away_col]]))
    ) %>%
    filter(Date_raw != "Date", Date_raw != "", Opp != "", Opp != "Opp")

  # Strip an optional leading weekday (e.g. "Thursday, Apr 2" -> "Apr 2")
  # and an optional trailing doubleheader marker (e.g. "Apr 2 (1)"/"Apr 2 (2)"
  # -> "Apr 2") before parsing with the season year (schedule pages omit the
  # year). Without stripping the doubleheader marker, game 2 of any remaining
  # doubleheader fails to parse, silently becomes NA, and gets dropped below
  # -- undercounting that team's remaining games by exactly one.
  date_clean  <- sub("^[A-Za-z]+,\\s*", "", df$Date_raw)
  date_clean  <- sub("\\s*\\([0-9]+\\)\\s*$", "", date_clean)
  parsed_date <- as.Date(paste(date_clean, SEASON), format = "%b %d %Y")

  n_unparsed <- sum(is.na(parsed_date))
  if (n_unparsed > 0) {
    cat(sprintf("  WARNING: %d row(s) failed date parsing and will be dropped:\n", n_unparsed))
    cat("   ", paste(df$Date_raw[is.na(parsed_date)], collapse = " | "), "\n")
  }

  df$Date    <- parsed_date
  df$is_home <- as.integer(df$home_marker != "@")
  df         <- df %>% select(Team, Date, Opp, is_home) %>% filter(!is.na(Date))

  df <- df %>% filter(Date > DEADLINE)

  cat(sprintf("  Remaining post-deadline games: %d\n", nrow(df)))
  all_teams[[code]] <- df

  Sys.sleep(4)
}

schedule_df <- bind_rows(all_teams)

# Franchise/abbreviation unification — same recodes used throughout the
# rest of this pipeline.
schedule_df$Team <- dplyr::recode(schedule_df$Team, "ATH" = "OAK")
schedule_df$Opp  <- dplyr::recode(schedule_df$Opp,  "ATH" = "OAK", "TBD" = "TBR", "FLA" = "MIA")

write.csv(schedule_df,
          "../data/schedule_2026_remaining.csv",
          row.names = FALSE)

cat(sprintf("\nSaved %d remaining-game rows to schedule_2026_remaining.csv\n", nrow(schedule_df)))
cat(sprintf("Teams with at least one remaining game: %d / %d\n",
            length(unique(schedule_df$Team)), length(TEAM_CODES)))
