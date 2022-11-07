library(rvest)
library(tidyverse)
library(RSelenium)
library(lubridate)
options(scipen=999)

robotstxt::get_robotstxt("https://www.refinery29.com")

nb_of_pages <- 53

get_article_info <- function(page_number){
  Sys.sleep(1)
  link <- paste0("https://www.refinery29.com/en-us/money-diary?page=", page_number)
  html <- link %>% 
    read_html() 
  title <- html %>%
    html_nodes(".title") %>%
    html_text()  %>%
    unlist()
  article_urls <- html %>%
    html_nodes(".card") %>%
    html_children() %>%
    html_attrs() %>%
    str_remove("href") %>%
    unlist()
  url <- str_c("https://www.refinery29.com", article_urls)
  tibble(url) %>%
    bind_cols(tibble(title) %>%
                filter(!title %in% c("All Money Diaries", "The Secret Sauce To A Successful Budget")))
}

article_title_url <- purrr::map_df(1:nb_of_pages, get_article_info) 

saveRDS(article_title_url, file = paste0("data/", today(),"-money-diaries-titles-and-url"))

extract_salary <- function(title) {
  
  if (str_detect(str_to_lower(title), "hour|month")) {
    title %>%
      str_to_lower() %>%
      str_remove_all(",") %>%
      str_extract("\\$\\d+(\\w|\\.\\d+)") %>%
      str_remove("\\$") %>%
      as.numeric()
  }
  
  else if (str_detect(str_to_lower(title), "million")) {
    title %>%
      str_to_lower() %>%
      str_extract("(\\d|\\.)+ million") %>%
      str_replace(" million", "00000") %>%
      str_replace("\\.", "") %>%
      as.numeric()
  }
  
  else { 
    title %>%
    str_to_lower() %>%
    str_remove_all(",") %>%
    str_extract("\\$\\d+\\w") %>%
    str_replace("k", "000") %>%
    str_remove("\\$") %>%
    as.numeric() 
  }
    
}

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
  mutate(state = str_extract(location, "(?<=, )\\w+"),
         city = str_extract(location, "^[^,]+")) %>%
  filter(str_detect(lowercase_title, "week"))

ggplot(money_data, aes(x = salary)) + 
  geom_histogram()

money_data %>%
  filter(salary == max(money_data$salary, na.rm = TRUE)) %>%
  pull(salary)

sum(is.na(money_data$salary)) / nrow(money_data)
# 2.8% are NA - not great but not terrible

ggplot(money_data, aes(x = salary)) + 
  geom_histogram() + 
  scale_x_log10(labels = scales::dollar) 

get_monthly_expenses <- function(url) {
  article_text <- url %>%
    read_html() %>%
    html_nodes(".section-text") %>%
    html_text()
  
  monthly_expenses <- grep('Monthly Expenses', article_text, value = TRUE)
  
  monthly_expenses %>%
    str_remove_all(",") %>%
    str_remove_all("Expenses") %>%
   # str_extract_all("\\w+: \\$\\d+") %>% 
    str_extract_all("[^.():\\d]*?: \\$\\d+") %>%
    unlist() 
}

all_monthly_expenses <- money_data %>% 
  mutate(monthly_expenses = map(url, get_monthly_expenses)) 

get_mortgage_rent <- function(df) {
  df %>%
    filter(str_detect(Type, "Rent|Mortgage"), !str_detect(Type, regex("insurance", ignore_case = TRUE))) %>%
    pull(Amount)
}

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

### Shiny app 


# include the images? 

pic_villagers <- villagers %>%
  mutate(
    picture = paste0(
      "<img src=\"",
      url,
      "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"",
      species,
      "\"></img>"
    )
  ) 

# Analysis ideas: What can we learn from 1,400 money diaries? Average monthly expenses, salaries from different states, % spent on rent or mortgage, net worth 