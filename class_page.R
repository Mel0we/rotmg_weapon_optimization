library(tidyverse)
library(rvest)

## Stats Table
# Read in class page
class_page <- read_html("https://www.realmeye.com/wiki/classes")

# Put all tables into a list
# This is done because the tables have no distinguishing features
class_table_list <- class_page |>
  html_elements('table')

# Extract relevant table from table list
stats_table <- html_table(class_table_list[[6]])

# Remove intermediary variables
rm(class_page, class_table_list)

## Class table is now complete :)