library(rvest)
library(tidyverse)
library(RSelenium)
library(lubridate)
options(scipen=999)
source("helper-functions.R")

robotstxt::get_robotstxt("https://www.refinery29.com")

nb_of_pages <- 53

article_title_url <- purrr::map_df(1:nb_of_pages, get_article_info) 

saveRDS(article_title_url, file = paste0("data/", today(),"-money-diaries-titles-and-url"))

money_data <- article_title_url %>% 
  mutate(lowercase_title = str_to_lower(title)) %>%
  mutate(salary = map_dbl(title, extract_salary),
         location = str_remove(title, "A Week In ") %>%
           str_remove("On .*"),
         joint = ifelse(str_detect(lowercase_title, "joint"), TRUE, FALSE),
         hourly = ifelse(str_detect(lowercase_title, "hour"), TRUE, FALSE),
         monthly = ifelse(str_detect(lowercase_title, "month"), TRUE, FALSE)) %>%
  # replace one broken url
  mutate(url = ifelse(url == "https://www.refinery29.com/en-us/money-diary-wyoming-education-program-specialist-salary", 
                      "https://www.refinery29.com/en-gb/money-diary-wyoming-education-program-specialist-salary",
                      url)) %>%
  # sometimes location is the state (e.g. North florida, Coastal Virginia)
  mutate(state = map_chr(location, extract_state),
         city = str_extract(location, "^[^,]+")) %>%
  filter(str_detect(lowercase_title, "week"))



all_monthly_expenses <- money_data %>% 
  mutate(monthly_expenses = map(url, get_monthly_expenses)) 


non_null_expenses <- all_monthly_expenses %>%
  filter(!map_lgl(monthly_expenses, is.null))

null_expenses <- all_monthly_expenses %>%
  filter(map_lgl(monthly_expenses, is.null))

share_for_housing <- non_null_expenses %>%
  unnest(monthly_expenses) %>%
  separate(monthly_expenses, into = c("Type", "Amount"), sep = ":") %>%
  mutate(Amount = str_remove(Amount, ",") %>%
           str_extract("\\d+")) %>%
  nest(monthly_expenses = c(Type, Amount)) %>%
  mutate(rent_mortgage = map(monthly_expenses, get_mortgage_rent)) %>%
  filter(map_int(rent_mortgage, length) == 1) %>%
  mutate(rent_mortgage = as.numeric(rent_mortgage)) %>%
  mutate(housing_share = case_when(monthly == TRUE ~ rent_mortgage / salary, 
                                   hourly == TRUE ~ rent_mortgage / (salary * 40 * 4),
                                   TRUE ~ (rent_mortgage * 12) / salary)) 

write_rds(share_for_housing, file = paste0("data/", today(), "-full-data"))
