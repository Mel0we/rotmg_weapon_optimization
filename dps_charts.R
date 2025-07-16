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
dps_matrix <- def_list

# Assign index of desired class/weapon for dps calculation
class_index <- 12
weapon_index <- 13

# Attempt to build function for weapon/class dps
# Not working right now, run as separate lines

# dps_calc <- function(class_index, weapon_index) {
  # average damage taken from dagger table at assigned index
  avg_dmg <- dagger_table$average_damage[weapon_index]
  # dmg_mult based on class atk stat and atk formula
  dmg_mult <- 0.5 + stats_table$ATT[class_index] / 50
  # num_shots will generally be 1 but can be higher
  # Each bullet affected by def individually
  num_shots <- dagger_table$num_shots[weapon_index] 
  # atks_per_sec based on class dex stat and attack speed formula
  atks_per_sec <- 1.5 + 6.5 * (stats_table$DEX[class_index] / 75)
  # Creates a vector of dps values based on the DEF list
  dps <- (avg_dmg * dmg_mult - def_list) * num_shots * atks_per_sec
# }

# Append dps from desired class/weapon combo onto dps_matrix
## Currently trying to append while renaming dps vector to weapon name
dps_matrix <- cbind(dps_matrix, dagger_table$Name[weapon_index] <- dps)

# Finally we can visualize the relationship
dps_matrix |> ggplot(
  aes(x = def_list, y = dps)) +
  geom_line(color = 'red') +
  labs(x = 'Enemy Defence', y = 'Damage per Second (DPS)') +
  theme_minimal()