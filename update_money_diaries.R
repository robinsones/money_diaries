# Incremental update: only scrape diaries we haven't seen before, parse them with
# the same logic as scraping_money_diaries.R, and append to the latest full dataset.
#
# Usage: Rscript update_money_diaries.R

suppressPackageStartupMessages({
  library(rvest)
  library(tidyverse)
  library(lubridate)
})
options(scipen = 999)
source("helper-functions.R")

max_pages <- 100 # safety cap on index pages to walk
stop_after_known_pages <- 2 # stop once this many consecutive pages have nothing new

latest_file <- function(suffix) {
  files <- list.files("data", pattern = paste0("^\\d{4}-\\d{2}-\\d{2}-", suffix, "$"), full.names = TRUE)
  if (length(files) == 0) stop("No data file found matching: ", suffix)
  sort(files) %>% last()
}

# ---- 1. Find index entries we haven't seen ----------------------------------

old_titles_path <- latest_file("money-diaries-titles-and-url")
old_full_path <- latest_file("full-data")
old_titles <- readRDS(old_titles_path)
old_full <- readRDS(old_full_path)
message("Previous index: ", old_titles_path, " (", nrow(old_titles), " entries)")
message("Previous data:  ", old_full_path, " (", nrow(old_full), " diaries)")

new_titles <- tibble(url = character(), title = character())
known_streak <- 0
for (page in seq_len(max_pages)) {
  page_info <- get_article_info(page)
  fresh <- page_info %>%
    filter(!url %in% old_titles$url, !url %in% new_titles$url)
  message("Page ", page, ": ", nrow(page_info), " entries, ", nrow(fresh), " new")
  new_titles <- bind_rows(new_titles, fresh)
  known_streak <- if (nrow(fresh) == 0) known_streak + 1 else 0
  if (known_streak >= stop_after_known_pages) break
}

if (nrow(new_titles) == 0) {
  message("No new diaries. Nothing to do.")
  quit(save = "no")
}

# ---- 2. Fetch article text for the new diaries only -------------------------

to_fetch <- parse_titles(new_titles)
message(nrow(new_titles), " new index entries, ", nrow(to_fetch), " look like diaries. Fetching...")

safe_get_article_text <- function(url) {
  Sys.sleep(1)
  tryCatch(get_article_text(url), error = function(e) {
    message("  FAILED: ", url, " (", conditionMessage(e), ")")
    NULL
  })
}

fetched <- to_fetch %>%
  mutate(article_text = map(url, safe_get_article_text))

failed <- fetched %>% filter(map_lgl(article_text, is.null))
fetched <- fetched %>% filter(!map_lgl(article_text, is.null))

# ---- 3. Parse and append -----------------------------------------------------

new_full <- parse_articles(fetched)
message(nrow(fetched), " fetched, ", nrow(new_full), " parsed into complete rows, ", nrow(failed), " failed")

full <- bind_rows(new_full, old_full) %>% distinct(url, .keep_all = TRUE)

# Failed fetches are left out of the index so the next run retries them
all_titles <- bind_rows(new_titles %>% filter(!url %in% failed$url), old_titles)

saveRDS(all_titles, paste0("data/", today(), "-money-diaries-titles-and-url"))
write_rds(full, paste0("data/", today(), "-full-data"))
message("Saved ", nrow(full), " diaries (", nrow(full) - nrow(old_full), " added) to data/", today(), "-full-data")
