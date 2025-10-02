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

dps_calc <- function(class, weapon) {
  avg_dmg <- dagger_table$average_damage[weapon]
  dmg_mult <- 0.5 + stats_table$ATT[class] / 50
  num_shots <- dagger_table$num_shots[weapon]
  atks_per_sec <- 1.5 + 6.5 * (stats_table$DEX[class] / 75)
  dps <- (avg_dmg * dmg_mult - def_list) * num_shots * atks_per_sec
  
  weapon_name <- dagger_table$Name[weapon]
  new_col <- setNames(data.frame(dps), weapon_name)
  dps_matrix <<- cbind(dps_matrix, new_col)
}

# Insert desired class/weapon combos to compare in comp_list
comp_list <- list(c(12, 13), c(12, 14), c(12, 15))
# The following loop applies those combos to the dps_calc function
for (i in comp_list) {
  dps_calc(i[1], i[2])
}

# Need to pivot to long data for visualization
dps_long <- pivot_longer(
  dps_matrix, 
  cols = -DEF,
  names_to = 'Weapon',
  values_to = 'DPS'
)

# Finally we can visualize the relationship
# Would like to reorder legend by order in dps_long, not alphabetical
dps_long |> ggplot(
  aes(x = DEF, y = DPS, color = Weapon)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_color_manual(
    values = c(
      'Dagger_of_Foul_Malevolence' = '#CB2929',
      'Dagger_of_Sinister_Deeds' = '#E97812',
      'Dagger_of_Dire_Hatred' = '#3750FB'
      ), 
    breaks = c(
      'Dagger_of_Dire_Hatred', 
      'Dagger_of_Sinister_Deeds', 
      'Dagger_of_Foul_Malevolence'
      )
  ) +
  labs(x = 'Enemy Defence', 
       y = 'Damage per Second (DPS)',
       title = 'Comparison of Trickster DPS w/ Tiered Daggers') +
  theme_minimal()
