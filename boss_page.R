library(tidyverse)
library(rvest)
library(polite)

## Boss Table
## Provides list of prominent defence values
## Scrapes from multiple html pages at once
## Done by creating a list of html links and 
## then looping thought them

## Acquire list of boss page URLs
# Establish root pages
realmeye_url <- 'https://www.realmeye.com'
dungeon_bosses_url <- 'https://www.realmeye.com/wiki/dungeon-bosses'

# Set up polite session
# Wouldn't want to break the wiki
# session <- bow(dungeon_bosses_url)

# Read the dungeon boss page
dungeon_bosses_page <- read_html(dungeon_bosses_url)

# Extract the link for each dungeon boss
boss_links <- dungeon_bosses_page |>
  html_elements('tr') |>
  html_elements('td:nth-child(1)') |>
  html_elements('a') |>
  html_attr('href')

# Concatenate base url w/ boss links
true_boss_links <- paste0(realmeye_url, boss_links)
true_boss_links <- true_boss_links[-147]

# Custom function to grab defence values from boss page
# Not currently working for pages that have multiple
# DEF instances (ex. segmented bosses)
scrape_boss_data <- function(x) {
  read_html(x) |>
  html_elements('h3#stats + p') |>
  html_text2() |>
  str_extract('DEF.*') |>
  str_extract_all('[0-9]') |>
  sapply(paste, collapse = '')
}

# testing
scrape_boss_data <- function(x) {
  read_html(true_boss_links[5]) |>
  html_elements('h3#stats + p') |>
  html_text2() |>
  str_extract('DEF.*') |>
  str_extract_all('[0-9]') |>
  sapply(paste, collapse = '')
}

# Runs DEF grabbing function on all links in boss list
# Sys.sleep to not overwhelm site
# Is grabbing 130~ values rather than whole list
# Need to investigate, having the custom function work
# for non-standard lists is priority
z <- vector()
for (x in true_boss_links) {
  Sys.sleep(2)
  z <- c(z, scrape_boss_data(x))
}
