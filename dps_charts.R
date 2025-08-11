library(tidyverse)

## DPS Charts
## Goal is to build linear regressions
## for some combinations of weapons across
## classes for enemies of varying defences.
## These can then be visualized w/ ggplot2
## and directly compared to find optimal
## weapons for certain bosses, dependent on class.

# General dmg formula needs to be made
# Overview:
# (avgdmg * dmg_mult - def) * num_shots * atks_per_sec

# Let's get a list of DEF values
# and then create a matrix to append weapon dps
def_list <- 0:60
dps_matrix <- as_tibble(def_list) |>
  rename(DEF = value)

# Assign index of desired class/weapon for dps calculation
# Can also make lists for multiple comparisons
class_index <- 12
weapon_index <- 13

# Attempt to build function for weapon/class dps
# Appends dps column to dps matrix
# and renames based on weapon name
## Rename functionality is not working yet
## Will come back to fix at some point

dps_calc <- function(class, weapon) {
  avg_dmg <- dagger_table$average_damage[weapon]
  dmg_mult <- 0.5 + stats_table$ATT[class] / 50
  num_shots <- dagger_table$num_shots[weapon]
  atks_per_sec <- 1.5 + 6.5 * (stats_table$DEX[class] / 75)
  dps <- (avg_dmg * dmg_mult - def_list) * num_shots * atks_per_sec
  dps_matrix <<- cbind(dps_matrix, dps)
  # weapon_name <- 'w'
  # assign(weapon_name, dagger_table$Name[weapon])
  # rename(dps_matrix, weapon_name = dps)
}
dps_calc(12, 13)


# Insert desired class/weapon combos to compare in comp_list
comp_list <- list(c(12, 13), c(12, 14), c(12, 15))
# The following loop applies those combos to the dps_calc function
## ^This does not work like desired
for (i in comp_list) {
  dps_calc(comp_list)
}


# Finally we can visualize the relationship
dps_matrix |> ggplot(
  aes(x = DEF, y = dps)) +
  geom_line(color = 'red') +
  labs(x = 'Enemy Defence', y = 'Damage per Second (DPS)') +
  theme_minimal()
