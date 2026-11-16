# Full re-scrape of every Money Diary. This re-downloads ~1,900 articles, so it is
# only needed to rebuild the data from scratch (for example after changing how
# articles are parsed). Day to day, update_money_diaries.R fetches just the new ones.

library(rvest)
library(tidyverse)
options(scipen = 999)
source("helper-functions.R")

robotstxt::get_robotstxt("https://www.refinery29.com")

nb_of_pages <- 70

article_title_url <- purrr::map_df(1:nb_of_pages, get_article_info) %>%
  distinct(url, .keep_all = TRUE) %>%
  # replace one broken url
  mutate(
    url = ifelse(
      url ==
        "https://www.refinery29.com/en-us/money-diary-wyoming-education-program-specialist-salary",
      "https://www.refinery29.com/en-gb/money-diary-wyoming-education-program-specialist-salary",
      url
    )
  )

all_article_text <- parse_titles(article_title_url) %>%
  mutate(article_text = map(url, function(url) {
    Sys.sleep(1)
    get_article_text(url)
  }))

# keep the raw text locally (not committed) so parsing can be re-run without re-scraping
saveRDS(all_article_text, paste0("data/", Sys.Date(), "-article-text.rds"))

diaries <- parse_articles(all_article_text) %>%
  select(all_of(diary_columns))

write_csv(article_title_url, "data/index.csv")
write_csv(diaries, "data/diaries.csv", na = "")
