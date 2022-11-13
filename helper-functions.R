get_article_info <- function(page_number){
  Sys.sleep(1)
  link <- paste0("https://www.refinery29.com/en-us/money-diary?page=", page_number)
  if (page_number == 1) {
    html <- link %>% 
      read_html() 
    
    title <- html %>%
      html_nodes(".title span") %>%
      html_text()  %>%
      unlist()
    
    regular_article_urls <- html %>%
      html_nodes(".card") %>%
      html_children() %>%
      html_attrs() %>%
      str_remove("href") %>%
      unlist()
    
    article_url_hero <- html %>%
      html_nodes(".hero-card-full-width") %>%
      html_children() %>%
      html_attrs() %>%
      str_remove("href") %>%
      unlist()
    
    article_urls <- c(article_url_hero, regular_article_urls)
    
    url <- str_c("https://www.refinery29.com", article_urls)
    tibble(url) %>%
      bind_cols(tibble(title) %>%
                  filter(!title %in% c("All Money Diaries", "The Secret Sauce To A Successful Budget")))
  }
  
  else {
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
}

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

extract_state <- function(location) {
  has_state_name <- length(str_which(location, state.name)) == 1
  if (has_state_name & !str_detect(location, "New York|Washington|Kansas City")) { 
    state_name <- str_extract(location, paste0(state.name, collapse = "|")) 
    state.abb[match(state_name, state.name)]
  }
  else if (str_detect(location, "New York City|NYC")) { 
    return("NY")
  }
  else { 
    str_extract(location, "(?<=, )\\w+")
  }
}

get_article_text <- function(url) { 
  url %>%
    read_html() %>%
    html_nodes(".section-text") %>%
    html_text()
}

get_monthly_expenses <- function(article_text) {
  monthly_expenses <- grep('Monthly Expenses', article_text, value = TRUE)
  
  monthly_expenses %>%
    str_remove_all(",") %>%
    str_remove_all("Expenses") %>%
    # str_extract_all("\\w+: \\$\\d+") %>% 
    str_extract_all("[^.():\\d]*?: \\$\\d+") %>%
    unlist() 
}

get_age <- function(article_text) { 
  age_text <- grep('Age:', article_text, value = TRUE)
  
  age <- age_text %>%
    str_extract("(?<=Age: ?)(\\d+)") %>%
    as.numeric()
  
  ifelse(length(age) == 0, NA, age)
}

get_occupation <- function(article_text) { 
  occupation_text <- grep('Occupation:', article_text, value = TRUE)
  
  occupation <- occupation_text %>%
    str_extract("(?<=Occupation: )([\\w\\s\\-]+)") %>%
    str_remove("Industry") %>%
    str_remove("Age$")
  
  ifelse(length(occupation) == 0, NA, occupation)
}

get_industry <- function(article_text) { 
  industry_text <- grep('Industry:', article_text, value = TRUE)
  
  industry <- industry_text %>%
    str_extract("(?<=Industry: )([\\w\\-\\s]+)") %>%
    str_remove("Age")
  
  ifelse(length(industry) == 0, NA, industry)
}

get_weekly_spend <- function(article_text) {
  total_spend <- grep('Daily Total:', article_text, value = TRUE)
  
  total_spend %>%
    str_extract_all("(?<=Daily Total: ?\\$)\\d+\\,?(\\d+)?\\.?(\\d+)?") %>%
    unlist() %>%
    str_remove(",") %>%
    as.numeric() %>%
    sum()
}

get_mortgage_rent <- function(df) {
  df %>%
    filter(str_detect(Type, "Rent|Mortgage"), !str_detect(Type, regex("insurance", ignore_case = TRUE))) %>%
    pull(Amount)
}
