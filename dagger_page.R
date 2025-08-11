library(tidyverse)
library(rvest)

## Dagger Table
# Read in dagger page
dagger_page <- read_html("https://www.realmeye.com/wiki/daggers")

# Put all tables into a list
# This is done because the tables have no distinguishing features
dagger_table_list <- dagger_page |>
  html_elements('table')

# Extract relevant table from table list
tiered_daggers <- html_table(dagger_table_list[[2]])
ut_daggers <- html_table(dagger_table_list[[3]])
st_daggers <- html_table(dagger_table_list[[4]])

# Combine tables
# All tables need to have same columns (same order not needed)
# Add missing columns to tiered_daggers
# Relocate new columns to standard position
tiered_daggers <- tiered_daggers |>
  mutate(`Shots(Arc Gap)` = 1,
         `Range(True Range)` = NA
  ) |>
  relocate(`Range(True Range)`, .before = `XP Bonus`) |>
  relocate(`Shots(Arc Gap)`, .before = `Range(True Range)`)

# Combine tiered, ut, and st lists
dagger_table <- rbind(tiered_daggers, ut_daggers, st_daggers)

# Remove intermediary objects
rm(dagger_page, dagger_table_list, 
   tiered_daggers, ut_daggers, st_daggers)

# Remove unnecessary columns
dagger_table <- dagger_table |>
  select(-c(Dagger, `Range(True Range)`, `XP Bonus`, `Feed Power`, Projectile))

# Remove problematic observations
# These have multiple shots with different average_damage/RoF
# Possible to include in future work
dagger_table <- dagger_table |>
  filter(!Name %in% c('Blade of Fates', 'Mischief', 'Soulcursed Scythe'))

# Clean up column names in dagger table
dagger_table <- dagger_table |>
  rename(average_damage = `Damage (Average)`) |>
  rename(num_shots = `Shots(Arc Gap)`)

# Eliminate spaces from weapon names
# This aids in completing the dps_charts later
dagger_table$Name <- gsub(' ', '_', dagger_table$Name)

# Create new columns (RoF, Stat Bonuses)
dagger_table <- dagger_table |>
  mutate(RoF = NA,
         stat_bonus = NA
  )

## Clean up column values in dagger table

# stat_bonus
# Create dummy vectors
w <- as.character(vector())
z <- as.character(vector())

# Loop through end of average_damage
# Picks up (+/-) text if present
for (x in dagger_table$average_damage) {
  w <- c(w, (substr(x, str_length(x) - 5, str_length(x))))
}

# Loop through end text for (+/-)
# If not present replace w/ NA
for (x in w) {
  ifelse('+' %in% substr(x, 1, 1) | '-' %in% substr(x, 1, 1),
         z <- c(z, x), 
         z <- c(z, NA)
  )
}
print(z)

# Replace stat_bonus column with new filtered vector
# Remove dummy variables
dagger_table$stat_bonus <- z
rm(w, x, z)

# RoF
# Acquire list of index palcement for each %, if present
index_list <- regexpr('%', dagger_table$average_damage)

# Create dummy vector
z <- as.character(vector())

# Populate dummy vector with specified text
# Loop through index list
# If index == -1, then default RoF
# If index != -1, grab substring based on % index
y = 1
for (x in index_list) {
  ifelse(x == -1, 
         z <- c(z, '105%'),
         z <- c(z, substr(dagger_table$average_damage[y], x - 3, x)))
  y = y + 1
}

# Some extracted substrings have leading spaces
# These need to be removed
z <- str_trim(z, 'left')

# These values should be converted to numerics for future use
# Drop the percentage sign
z <- str_replace(z, '%', '')
# Convert to numeric
z <- as.numeric(z)
# Convert to percentage represented as whole number
z <- z / 100

# Replace RoF column with new filtered vector
# Remove dummy variables
dagger_table$RoF <- z
rm(x, y, z, index_list)

# num_shots
# Create dummy vector
z <- as.character(vector())

# Populate vector with list of shot numbers
# Takes index[1] of each row in num_shots
# This removes angle values
for (x in dagger_table$num_shots) {
  z <- c(z, (substr(x, 1, 1)))
}

# Populate num_shots with filtered vector
# and convert to appropriate data type
dagger_table$num_shots <- z |>
  as.numeric()

# Remove dummy variables
rm(x, z)

# average_damage
# Create dummy vectors
y = vector()
z = vector()

# Acquire list of index placement for each '(' and ')', if present
open <- regexpr('(', dagger_table$average_damage, fixed = TRUE)
close <- regexpr(')', dagger_table$average_damage, fixed = TRUE)

# Get average dmg values in a vector
# Loop through average_damage, using indices of '(' & ')'
# w cycle count to account for index of average_damage vector
w = 1
for (x in dagger_table$average_damage) {
  y <- c(y, (substr(dagger_table$average_damage[w], open[w], close[w])))
  w = w + 1
}

# Remove waste from average_damage list y
# Retains only numeric characters and '.'
for (x in y) {
  z <- c(z, (gsub('[^0-9.]', '', x)))
}

# Replace average_damage column in dagger table with filtered vector
# and convert column to appropriate data type
dagger_table$average_damage <- z |>
  as.numeric()

# Remove intermediary variables
rm(w, x, y, z, open, close)

## Dagger table is now complete :)
