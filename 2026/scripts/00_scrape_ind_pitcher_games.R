library(httr)
library(rvest)
library(dplyr)

#
#  BATCH PITCHER GAME SCRAPER
#  We tried running thru this to scrape all years, but the way the website is structured
#  We hit a limit for this data... we avoided this by only grabbing two years at a time
#  Run in batches of 2 years at a time, restarting R between each batch.
#  Change BATCH_START and BATCH_END before each run.
#
#  batches:
#    Batch 1:  2005-2006
#    Batch 2:  2007-2008
#    Batch 3:  2009-2010
#    Batch 4:  2011-2012
#    Batch 5:  2013-2014
#    Batch 6:  2015-2016
#    Batch 7:  2017-2018
#    Batch 8:  2019-2020
#    Batch 9:  2021-2022
#    Batch 10: 2023-2024
#    Batch 11: 2025-2025
#    Batch 12: 2026-2026



BATCH_START <- 2026   #change this before each run
BATCH_END   <- 2026   #change this before each run

# Set STATHEAD_USERNAME / STATHEAD_PASSWORD in a local .env (see .env.example)
USERNAME <- Sys.getenv("STATHEAD_USERNAME")
PASSWORD <- Sys.getenv("STATHEAD_PASSWORD")

#Login
h <- handle("https://www.sports-reference.com")
login_response <- POST(
  url    = "https://www.sports-reference.com/users/login.cgi",
  handle = h,
  body   = list(
    username = USERNAME,
    password = PASSWORD,
    login    = "Login"
  ),
  encode = "form",
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    `Referer`    = "https://www.sports-reference.com/users/login.cgi"
  )
)

cat("Login status:", status_code(login_response), "\n")
login_text <- content(login_response, "text", encoding = "UTF-8")

#checking if we got logged out
if (grepl("logout", login_text, ignore.case = TRUE)) {
  cat("Login successful!\n")
} else {
  cat("Login failed — saving debug HTML.\n")
  writeLines(login_text, "debug_login.html")
  stop("Stopping: not logged in.")
}

BASE_URL <- "https://www.sports-reference.com/stathead/baseball/player-pitching-game-finder.cgi"

# ── SCRAPE YEAR BY YEAR ───────────────────────────────────────────────────────
all_years <- list()

for (yr in BATCH_START:BATCH_END) {
  cat(sprintf("\n========== YEAR %d ==========\n", yr))

  year_data <- list()
  offset    <- 0
  page_num  <- 1

  base_params <- list(
    request      = "1",
    order_by_asc = "1",
    order_by     = "date",
    timeframe    = "custom_timeframe",
    date_min     = paste0(yr, "-03-01"),   # start of season (spring training games)
    date_max     = if (yr == 2026) paste0(yr, "-08-03") else paste0(yr, "-07-31"),  # trade deadline cutoff (2026 moved to Aug 3)
    "ccomp[1]"   = "lt",
    "cval[1]"    = "100",
    "cstat[1]"   = "p_wpa_def"
  )

  repeat {
    cat(sprintf("  Page %d (offset %d)...\n", page_num, offset))

    params <- c(base_params, list(offset = offset))

    resp <- GET(
      url    = BASE_URL,
      handle = h,
      query  = params,
      add_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        `Referer`    = "https://www.sports-reference.com/stathead/"
      )
    )

    cat("  HTTP status:", status_code(resp), "\n")

    if (status_code(resp) != 200) {
      cat("  Non-200 response — saving debug and stopping.\n")
      writeLines(content(resp, "text", encoding = "UTF-8"),
                 sprintf("debug_%d_page_%d.html", yr, page_num))
      break
    }

    page_html  <- read_html(content(resp, "text", encoding = "UTF-8"))
    table_node <- html_element(page_html, "#stats")

    if (is.na(table_node) || is.null(table_node)) {
      cat("  No table found — moving to next year.\n")
      writeLines(content(resp, "text", encoding = "UTF-8"),
                 sprintf("debug_%d_page_%d.html", yr, page_num))
      break
    }

    df <- html_table(table_node, header = TRUE)
    df <- df[df[[1]] != names(df)[1], ]   # remove repeated header rows
    df$scrape_year <- yr

    cat(sprintf("  Rows: %d\n", nrow(df)))

    if (nrow(df) == 0) {
      cat("  Empty — done with year.\n")
      break
    }

    year_data[[page_num]] <- df

    if (nrow(df) < 200) {
      cat("  Last page for this year.\n")
      break
    }

    offset   <- offset + 200
    page_num <- page_num + 1
    Sys.sleep(4)
  }

  if (length(year_data) > 0) {
    all_years[[as.character(yr)]] <- bind_rows(year_data)
    cat(sprintf("  Year %d total rows: %d\n", yr,
                nrow(all_years[[as.character(yr)]])))
  }

  # Checkpoint save after every year — batch specific filename
  if (length(all_years) > 0) {
    checkpoint <- bind_rows(all_years)
    write.csv(checkpoint,
              sprintf("../data/pitcher_batch_%d_%d.csv", BATCH_START, BATCH_END),
              row.names = FALSE)
  }

  Sys.sleep(8)
}

# ── FINAL SAVE FOR THIS BATCH ─────────────────────────────────────────────────
if (length(all_years) > 0) {
  final_df <- bind_rows(all_years)
  cat(sprintf("\nBatch %d-%d complete.\n", BATCH_START, BATCH_END))
  cat(sprintf("Total rows: %d\n", nrow(final_df)))
  write.csv(final_df,
            sprintf("../data/pitcher_batch_%d_%d.csv", BATCH_START, BATCH_END),
            row.names = FALSE)
  cat(sprintf("Saved to pitcher_batch_%d_%d.csv\n", BATCH_START, BATCH_END))
  cat("\nRestart RStudio before running the next batch.\n")
} else {
  cat("No data collected.\n")
}
