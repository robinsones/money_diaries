Refinery29 is a digital media and entertainment website that publishes "Money Diaries" a few times a week, where a reader submits their income, net worth, monthly expenses, and a diary of their spending for a week. There is no search function for money diaries, so if you were interested in only diaries from New York City or only people making less than $50,000, you would have to manually search through the hundreds of past diaries. 

I made a [dashboard](https://robinsones.github.io/money_diaries/) where anyone can search and filter past money diaries by salary, location, age, occupation, weekly spend, and monthly rent/mortgage. Information is scraped from Refinery29's website using rvest and then parsed with dplyr/purrr/stringr.

## How it stays up to date

A [GitHub Actions workflow](.github/workflows/update.yml) runs every Monday. It:

1. runs `update_money_diaries.R`, which checks the Money Diaries index for articles it hasn't seen, fetches only those (one request per second), and appends the parsed results to `data/diaries.csv`
2. commits the updated data
3. re-renders the dashboard (`shiny.Rmd`) and publishes it to GitHub Pages
4. runs a health check: if fewer than 6 of the 10 most recent diaries could be parsed, Refinery29 has probably changed their article template, so the run fails and opens an issue

Each run's summary on the Actions tab lists what was added and what couldn't be parsed.

## Files

- `helper-functions.R`: scraping and parsing functions
- `update_money_diaries.R`: the weekly incremental update
- `scraping_money_diaries.R`: full re-scrape of every diary, only needed to rebuild from scratch
- `shiny.Rmd`: the dashboard
- `data/index.csv`: every article seen on the index; `data/diaries.csv`: the parsed diaries
