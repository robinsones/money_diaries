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
  
  # Since ~2023 the whole intro (salary, assets, debt, expenses) is one block of
  # text, so only keep what comes after the "Monthly Expenses" header. Otherwise
  # e.g. a mortgage balance listed under Debt gets picked up as the monthly payment.
  monthly_expenses %>%
    str_extract(regex("Monthly Expenses.*", dotall = TRUE)) %>%
    str_remove_all(",") %>%
    str_remove_all("Monthly Expenses") %>%
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

# Since ~2023 diaries have a single "Housing Costs:" line instead of separate
# Rent / Mortgage lines, and it is often a sentence ("I pay $1,325 in rent...")
# rather than a plain amount. Take the first dollar amount on that line.
get_housing_costs <- function(article_text) {
  housing_text <- article_text %>%
    str_extract(regex("Housing Costs?:.{0,300}", ignore_case = TRUE, dotall = TRUE)) %>%
    discard(is.na)
  
  if (length(housing_text) == 0) {
    return(NA_real_)
  }
  
  housing_text[1] %>%
    # stop at the next line of the template so we don't pick up a loan payment
    str_remove(regex("Loan Payments?:.*", ignore_case = TRUE, dotall = TRUE)) %>%
    str_remove_all(",") %>%
    # skip totals like "$1.4 million" (remaining mortgage, home value)
    str_extract("\\$(?>\\d+(?:\\.\\d+)?)(?! ?(million|k\\b))") %>%
    str_remove("\\$") %>%
    as.numeric()
}

# Same parsing as scraping_money_diaries.R, applied to a tibble of url + title.
# Every step is row-wise, so running it on only the new rows gives the same result
# as re-running it on everything.
parse_titles <- function(article_title_url) {
  article_title_url %>%
    mutate(lowercase_title = str_to_lower(title)) %>%
    mutate(
      salary = map_dbl(title, extract_salary),
      location = str_remove(title, "A Week In ") %>%
        str_remove("On .*"),
      joint = str_detect(lowercase_title, "joint"),
      hourly = str_detect(lowercase_title, "hour"),
      monthly = str_detect(lowercase_title, "month")
    ) %>%
    mutate(
      state = map_chr(location, extract_state),
      city = str_extract(location, "^[^,]+")
    ) %>%
    filter(str_detect(lowercase_title, "week"))
}

parse_articles <- function(articles) {
  articles %>%
    mutate(monthly_expenses = map(article_text, get_monthly_expenses)) %>%
    filter(!str_detect(lowercase_title, "couple's|couples|5 money diaries")) %>%
    filter(!str_detect(url, "comparison")) %>%
    mutate(
      age = map_dbl(article_text, get_age),
      occupation = map_chr(article_text, get_occupation),
      industry = map_chr(article_text, get_industry),
      total_weekly_spend = map_dbl(article_text, get_weekly_spend) %>% na_if(0)
    ) %>%
    # keep_empty: newer diaries can have a housing cost without itemised expenses
    unnest(monthly_expenses, keep_empty = TRUE) %>%
    separate(monthly_expenses, into = c("Type", "Amount"), sep = ":") %>%
    mutate(Amount = str_remove(Amount, ",") %>% str_extract("\\d+")) %>%
    nest(monthly_expenses = c(Type, Amount)) %>%
    mutate(
      housing_costs = map_dbl(article_text, get_housing_costs),
      rent_mortgage = map(monthly_expenses, get_mortgage_rent)
    ) %>%
    # prefer the "Housing Costs" line; fall back to a single Rent / Mortgage line
    filter(!is.na(housing_costs) | map_int(rent_mortgage, length) == 1) %>%
    mutate(
      rent_mortgage = map2_dbl(housing_costs, rent_mortgage, ~ if (!is.na(.x)) .x else as.numeric(.y))
    ) %>%
    select(-housing_costs) %>%
    mutate(
      housing_share = case_when(
        monthly ~ rent_mortgage / salary,
        hourly ~ rent_mortgage / (salary * 40 * 4),
        TRUE ~ (rent_mortgage * 12) / salary
      )
    )
}
