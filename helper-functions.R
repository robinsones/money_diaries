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

# ---- State extraction ---------------------------------------------------------

us_states <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")

canadian_provinces <- c(
  "British Columbia" = "BC", "Alberta" = "AB", "Saskatchewan" = "SK",
  "Manitoba" = "MB", "Ontario" = "ON", "Quebec" = "QC", "Nova Scotia" = "NS",
  "New Brunswick" = "NB", "Newfoundland" = "NL", "Prince Edward Island" = "PE"
)

# Educated guesses for titles that only name a city or region ("A Week In Seattle").
# Names are regular expressions matched on word boundaries; first match wins.
place_to_state <- c(
  # New York
  "New York|NYC|Brooklyn|Manhattan|Queens|The Bronx|Staten Island|Long Island|Harlem" = "NY",
  # California
  "Los Angeles|L\\.A\\.|LA|San Francisco|Bay Area|Silicon Valley|San Diego|San Jose|Oakland|Sacramento|Orange County" = "CA",
  "Boston|Cambridge" = "MA",
  "Chicago" = "IL",
  "Philadelphia|Philly|Pittsburgh" = "PA",
  "Seattle" = "WA",
  "Denver|Rocky Mountains" = "CO",
  "Dallas|Houston|Austin|San Antonio|Fort Worth" = "TX",
  "Atlanta" = "GA",
  "Miami|Orlando|Tampa" = "FL",
  "Detroit" = "MI",
  "Minneapolis|St\\.? Paul|Twin Cities" = "MN",
  "Milwaukee" = "WI",
  "Cincinnati|Cleveland|Columbus" = "OH",
  "Indianapolis" = "IN",
  "Nashville|Memphis" = "TN",
  "Baltimore" = "MD",
  "Las Vegas" = "NV",
  "Salt Lake City" = "UT",
  "Portland" = "OR",
  "Phoenix" = "AZ",
  "New Orleans" = "LA",
  "Kansas City|St\\.? Louis" = "MO",
  "Charlotte|Raleigh|Durham" = "NC",
  # Canada
  "Toronto|Ottawa|Hamilton" = "ON",
  "Vancouver" = "BC",
  "Montreal" = "QC",
  "Halifax" = "NS",
  "Saskatoon" = "SK",
  "Edmonton|Calgary" = "AB",
  "Canada|The Prairies" = "Canada",
  # Rest of the world: use the country
  "London|England|Scotland" = "United Kingdom",
  "Paris" = "France",
  "Berlin" = "Germany",
  "Copenhagen" = "Denmark",
  "Amsterdam|The Netherlands" = "Netherlands",
  "Brussels|Belgium" = "Belgium",
  "Vienna" = "Austria",
  "Jerusalem" = "Israel",
  "Shanghai" = "China",
  "Mexico City" = "Mexico",
  "Singapore" = "Singapore",
  "Hong Kong" = "Hong Kong",
  "Bahrain" = "Bahrain",
  "Luxembourg" = "Luxembourg",
  "Morocco" = "Morocco",
  "Ecuador" = "Ecuador",
  "South Korea" = "South Korea",
  "Australia" = "Australia"
)

# Pulls the place out of a title. Most are "A Week In <place> On A $<income>", but
# some put the place elsewhere ("A Week On Unemployment In Portland, Oregon").
extract_location <- function(title) {
  if (str_detect(title, "^A Week In ")) {
    location <- title %>%
      str_remove("^A Week In ") %>%
      str_remove("On .*")
  } else {
    location <- str_remove(title, ",? On (A |An )?(Joint )?\\$.*")
    if (str_detect(location, " [Ii]n ")) {
      # "A Week As A Student In Los Angeles" -> "Los Angeles"
      location <- str_extract(location, "(?<= [Ii]n )(?!.* [Ii]n ).*")
    } else {
      # "A Week On Long Island" -> "Long Island"
      location <- str_remove(location, "^A Week (On|Near) ")
    }
  }
  
  location <- location %>%
    # "New York As An Intern" -> "New York"
    str_remove(" As An? .*") %>%
    # "Michigan That Costs $3,500 A Month" -> "Michigan"
    str_remove(",? That Costs .*") %>%
    str_squish() %>%
    str_remove(",$")
  
  # no place in the title at all ("A Week As A Nomad")
  if (str_detect(location, "^(A Week|How Much)")) {
    return(NA_character_)
  }
  location
}

# Returns a US state / Canadian province abbreviation, a country name for
# everywhere else, or NA when the title doesn't say (e.g. "The Midwest").
extract_state <- function(location) {
  if (is.na(location)) {
    return(NA_character_)
  }
  
  location <- location %>%
    str_squish() %>%
    str_remove(",$")
  
  # "Washington, D.C." in all its spellings
  if (str_detect(location, "Washington,? D\\.?C|^D\\.?C\\.?$")) {
    return("DC")
  }
  
  # 1. Whatever follows the last comma: "Dallas, TX", "Durham, N.C.", "Ottawa, Ontario"
  after_comma <- str_extract(location, "(?<=,)[^,]*$") %>% str_trim()
  if (!is.na(after_comma)) {
    abbreviation <- after_comma %>% str_remove_all("\\.") %>% str_extract("^[A-Z]{2}\\b")
    if (abbreviation %in% c(us_states, canadian_provinces)) {
      return(abbreviation)
    }
    if (after_comma %in% names(c(us_states, canadian_provinces))) {
      return(unname(c(us_states, canadian_provinces)[after_comma]))
    }
  }
  
  # An abbreviation elsewhere: "Austin TX", "Jersey City, NJ, A $82,500 Salary"
  other_abbreviation <- str_extract(location, "(?<=, )[A-Z]{2}\\b|\\b[A-Z]{2}$")
  if (other_abbreviation %in% c(us_states, canadian_provinces)) {
    return(other_abbreviation)
  }
  
  # 2. A state name anywhere: "Northern Virginia", "Upstate New York". Longest name
  # first so "West Virginia" beats "Virginia". Skip names that are usually a city.
  all_names <- c(us_states, canadian_provinces)
  state_names <- names(all_names)[order(-nchar(names(all_names)))]
  state_name <- str_extract(location, paste0("\\b(", paste0(state_names, collapse = "|"), ")"))
  is_city <- str_detect(location, "Kansas City") |
    (identical(state_name, "Washington") & !str_detect(location, "Washington State"))
  if (!is.na(state_name) & !is_city) {
    return(unname(all_names[state_name]))
  }
  
  # 3. Educated guess from a well-known city or region
  for (pattern in names(place_to_state)) {
    if (str_detect(location, paste0("\\b(", pattern, ")(?!\\w)"))) {
      return(unname(place_to_state[pattern]))
    }
  }
  
  # 4. Otherwise assume "City, Country" (but not a sentence that happens to have a comma)
  if (!is.na(after_comma) & str_detect(location, "^(?!A Week)[^,]+, [A-Z]\\w+( \\w+){0,2}$")) {
    return(after_comma)
  }
  
  NA_character_
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
      location = map_chr(title, extract_location),
      joint = str_detect(lowercase_title, "joint"),
      hourly = str_detect(lowercase_title, "hour"),
      monthly = str_detect(lowercase_title, "month")
    ) %>%
    mutate(
      state = map_chr(location, extract_state),
      # titles like "A Week On Unemployment In Portland, Oregon" leave no usable
      # location, so fall back to the title with the income part removed
      state = coalesce(
        state,
        map_chr(str_remove(title, " On (A |An )?(Joint )?\\$.*"), extract_state)
      ),
      city = str_extract(location, "^[^,]+")
    ) %>%
    filter(str_detect(lowercase_title, "week"))
}

# Round-ups and comparison pieces that sit on the index but aren't single diaries
is_excluded_diary <- function(lowercase_title, url) {
  str_detect(lowercase_title, "couple's|couples|5 money diaries") | str_detect(url, "comparison")
}

parse_articles <- function(articles) {
  articles %>%
    mutate(monthly_expenses = map(article_text, get_monthly_expenses)) %>%
    filter(!is_excluded_diary(lowercase_title, url)) %>%
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

# ---- Stored data ---------------------------------------------------------------

# Columns kept in data/diaries.csv. Article text is deliberately not stored.
diary_columns <- c(
  "url", "title", "salary", "location", "state", "city", "joint", "hourly", "monthly",
  "age", "occupation", "industry", "total_weekly_spend", "rent_mortgage", "housing_share"
)

read_diaries <- function(path = "data/diaries.csv") {
  read_csv(
    path,
    col_types = cols(
      url = "c", title = "c", location = "c", state = "c", city = "c",
      occupation = "c", industry = "c",
      joint = "l", hourly = "l", monthly = "l",
      .default = "d"
    )
  )
}
