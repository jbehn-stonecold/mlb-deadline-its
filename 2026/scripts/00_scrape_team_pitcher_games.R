library(httr)
library(rvest)
library(dplyr)

# --- CREDENTIALS ---
# Set STATHEAD_USERNAME / STATHEAD_PASSWORD in a local .env (see .env.example)
USERNAME <- Sys.getenv("STATHEAD_USERNAME")
PASSWORD <- Sys.getenv("STATHEAD_PASSWORD")

# --- LOGIN ---
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
if (grepl("logout", login_text, ignore.case = TRUE)) {
  cat("Login successful!\n")
} else {
  cat("Login failed — saving debug HTML.\n")
  writeLines(login_text, "debug_login.html")
  stop("Stopping: not logged in.")
}

# --- BASE URL ---
BASE_URL <- "https://www.sports-reference.com/stathead/baseball/team-pitching-game-finder.cgi"

# --- SCRAPE ONE YEAR AT A TIME ---
all_years <- list()

for (yr in 2005:2026) {
  cat(sprintf("\n========== YEAR %d ==========\n", yr))
  
  year_data <- list()
  offset    <- 0
  page_num  <- 1
  
  base_params <- list(
    request      = "1",
    order_by_asc = "1",
    order_by     = "date",
    timeframe    = "seasons",
    year_min     = as.character(yr),
    year_max     = as.character(yr),
    "ccomp[1]"   = "lt",
    "cval[1]"    = "2.1",
    "cstat[1]"   = "b_wpa"
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
      cat("  Non-200 — saving debug and stopping.\n")
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
    df <- df[df[[1]] != names(df)[1], ]  # remove repeated header rows
    df$scrape_year <- yr                  # tag the year for safety
    
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
    Sys.sleep(4)  # be polite to the server
  }
  
  if (length(year_data) > 0) {
    all_years[[as.character(yr)]] <- bind_rows(year_data)
    cat(sprintf("  Year %d total rows: %d\n", yr, nrow(all_years[[as.character(yr)]])))
  }
  
  # Save a checkpoint after every year in case something breaks mid-run
  if (length(all_years) > 0) {
    checkpoint <- bind_rows(all_years)
    write.csv(checkpoint, "../data/stathead_team_pitching_checkpoint.csv", row.names = FALSE)
  }
  
  Sys.sleep(8)  # longer pause between years
}

# --- FINAL SAVE ---
if (length(all_years) > 0) {
  final_df <- bind_rows(all_years)
  cat(sprintf("\nTotal rows: %d\n", nrow(final_df)))
  write.csv(final_df, "../data/stathead_team_pitching.csv", row.names = FALSE)
  cat("Saved to stathead_team_pitching.csv\n")
} else {
  cat("No data collected.\n")
}
