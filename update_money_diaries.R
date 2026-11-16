# Incremental update: scrape only the diaries we haven't seen before, parse them,
# and append them to data/diaries.csv. Run weekly by .github/workflows/update.yml.
#
# Usage: Rscript update_money_diaries.R
#
# data/index.csv    every entry seen on the Money Diaries index pages (url, title)
# data/diaries.csv  one row per successfully parsed diary; this is what the app shows

suppressPackageStartupMessages({
  library(rvest)
  library(tidyverse)
})
options(scipen = 999)
source("helper-functions.R")

index_path <- "data/index.csv"
diaries_path <- "data/diaries.csv"

max_pages <- 100 # safety cap on index pages to walk
stop_after_known_pages <- 2 # stop once this many consecutive pages have nothing new

# Health check: of the most recent diaries on the index, how many made it into
# diaries.csv? A low number means Refinery29 changed their template again.
health_window <- 10
health_minimum <- 6

# ---- 1. Find index entries we haven't seen ----------------------------------

old_index <- read_csv(index_path, col_types = "cc")
old_diaries <- read_diaries(diaries_path)
message("Index: ", nrow(old_index), " entries. Diaries: ", nrow(old_diaries), ".")

new_index <- tibble(url = character(), title = character())
known_streak <- 0
for (page in seq_len(max_pages)) {
  page_info <- get_article_info(page)
  fresh <- page_info %>%
    filter(!url %in% old_index$url, !url %in% new_index$url)
  message("Page ", page, ": ", nrow(page_info), " entries, ", nrow(fresh), " new")
  new_index <- bind_rows(new_index, fresh)
  known_streak <- if (nrow(fresh) == 0) known_streak + 1 else 0
  if (known_streak >= stop_after_known_pages) break
}

# ---- 2. Fetch and parse the new diaries --------------------------------------

safe_get_article_text <- function(url) {
  Sys.sleep(1)
  tryCatch(get_article_text(url), error = function(e) {
    message("  FAILED: ", url, " (", conditionMessage(e), ")")
    NULL
  })
}

fetched <- parse_titles(new_index) %>%
  mutate(article_text = map(url, safe_get_article_text))

failed <- fetched %>% filter(map_lgl(article_text, is.null))
fetched <- fetched %>% filter(!map_lgl(article_text, is.null))
new_diaries <- parse_articles(fetched) %>% select(all_of(diary_columns))
unparsed <- fetched %>% filter(!url %in% new_diaries$url)

# ---- 3. Save -----------------------------------------------------------------

# Failed fetches are left out of the index so the next run retries them
index <- bind_rows(new_index %>% filter(!url %in% failed$url), old_index)
diaries <- bind_rows(new_diaries, old_diaries) %>% distinct(url, .keep_all = TRUE)

if (nrow(new_index) > 0) {
  write_csv(index, index_path)
  write_csv(diaries, diaries_path, na = "")
}

# ---- 4. Report and health check ----------------------------------------------

recent <- parse_titles(index) %>%
  filter(!is_excluded_diary(lowercase_title, url)) %>%
  head(health_window) %>%
  mutate(parsed = url %in% diaries$url)
healthy <- sum(recent$parsed) >= health_minimum

bullet_list <- function(df) {
  if (nrow(df) == 0) return("- none")
  paste0("- [", df$title, "](", df$url, ")", collapse = "\n")
}

summary <- paste0(
  "## Money Diaries update\n\n",
  "- New index entries: ", nrow(new_index), "\n",
  "- New diaries added: ", nrow(new_diaries), "\n",
  "- Total diaries: ", nrow(diaries), "\n",
  "- Health check: ", sum(recent$parsed), " of the ", nrow(recent), " most recent diaries parsed (minimum ",
  health_minimum, ") ", if (healthy) "✅" else "❌", "\n\n",
  "### Added\n", bullet_list(new_diaries), "\n\n",
  "### Fetched but not parsed\n", bullet_list(unparsed), "\n\n",
  "### Failed to fetch (will retry)\n", bullet_list(failed), "\n"
)
message("\n", summary)

# On GitHub Actions, show the report on the run page and pass results to later steps
if (Sys.getenv("GITHUB_STEP_SUMMARY") != "") {
  write_lines(summary, Sys.getenv("GITHUB_STEP_SUMMARY"), append = TRUE)
  write_lines(
    c(
      paste0("new_diaries=", nrow(new_diaries)),
      paste0("healthy=", tolower(healthy))
    ),
    Sys.getenv("GITHUB_OUTPUT"),
    append = TRUE
  )
  if (!healthy) {
    write_lines(
      c(
        paste0(
          "Only ", sum(recent$parsed), " of the ", nrow(recent),
          " most recent Money Diaries could be parsed. Refinery29 has probably changed ",
          "the article template, so the parsing functions in `helper-functions.R` need updating.\n"
        ),
        "### Most recent diaries\n",
        paste0("- ", if_else(recent$parsed, "✅", "❌"), " [", recent$title, "](", recent$url, ")")
      ),
      "health-report.md"
    )
  }
}
