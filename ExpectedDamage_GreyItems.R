############################################################################################################################################
# CRYSTAL GUARDIANS ANALYSIS
############################################################################################################################################

# Started: 2024-01-15
# Purpose: While not comprehensive, I'd like to understand the optimal configuration of grey (most common) items with respect to a maxed-out
#    electric crystal progression on a projectile electricity tower. The reason for this analysis is based both on general curiosity for
#    an 'optimal' strategy given a grey-item constraint and to unerstand how 'broken' certain combinations can be. Knowing this would allow
#    for easier time-record achievements since very little thought would need to go into the selection process.

############################################################################################################################################
# LOAD LIBRARIES
############################################################################################################################################

library(tidyverse)


############################################################################################################################################
# SETUP
############################################################################################################################################

#### CALCULATIONS ####

# All stats and calculations come from the game as of build 13170048

# Base stats
DAMAGE_DEFAULT <- 60
CRIT_CHANCE_DEFAULT <- 0.25
CRIT_DAMAGE_DEFAULT <- 1.4
ATTACK_SPEED_DEFAULT <- 3.1

ELECTRICITY_CRIT_BOOST <- 1.4
PERK_CRIT_BOOST <- 1.5
total_crit_boost <- ELECTRICITY_CRIT_BOOST + PERK_CRIT_BOOST

# Two grey items get boosts based on low hp. This analysis is assuming you 'game' it such that you have min survivable hp
TOWER_MAX_HP <- 2000
TOWER_HP <- 200
tower_hp_pct <- TOWER_HP / TOWER_MAX_HP
missing_health_multiplier <- (1 - tower_hp_pct) * 10 # 10 to indicate number of 10% chunks missing


flat_damage <- function(item1_damage, item2_damage, item3_damage, item4_damage, item5_damage, item6_damage, item7_damage) {
  damage_boost <- item1_damage + item2_damage + item3_damage + item4_damage + item5_damage + item6_damage + item7_damage
  flat_damage_output <- DAMAGE_DEFAULT * (1 + damage_boost)
  return(flat_damage_output)
}


crit_chance <- function(item1_crit_chance, item2_crit_chance, item3_crit_chance, item4_crit_chance, item5_crit_chance, item6_crit_chance, item7_crit_chance) {
  crit_chance_bonus <- item1_crit_chance + item2_crit_chance + item3_crit_chance + item4_crit_chance + item5_crit_chance + item6_crit_chance + item7_crit_chance
  crit_chance_output <- CRIT_CHANCE_DEFAULT + crit_chance_bonus
  return(crit_chance_output)
}


crit_damage <- function(item1_crit_damage, item2_crit_damage, item3_crit_damage, item4_crit_damage, item5_crit_damage, item6_crit_damage, item7_crit_damage) {
  crit_damage_bonus <- item1_crit_damage + item2_crit_damage + item3_crit_damage + item4_crit_damage + item5_crit_damage + item6_crit_damage + item7_crit_damage
  crit_damage_output <- CRIT_DAMAGE_DEFAULT + crit_damage_bonus * total_crit_boost
  return(crit_damage_output)
}


attack_speed <- function(item1_attack_speed, item2_attack_speed, item3_attack_speed, item4_attack_speed, item5_attack_speed, item6_attack_speed, item7_attack_speed) {
  attack_speed_bonus <- item1_attack_speed + item2_attack_speed + item3_attack_speed + item4_attack_speed + item5_attack_speed + item6_attack_speed + item7_attack_speed
  attack_speed_output <- ATTACK_SPEED_DEFAULT * (1 + attack_speed_bonus) 
  return(attack_speed_output)
}


expected_damage <- function(flat_damage, crit_chance, crit_damage, attack_speed) {
  capped_crit_chance <- ifelse(crit_chance >= 1, 1, crit_chance) # sets max crit chance at 100%
  non_crit_damage <- flat_damage * (1 - capped_crit_chance)
  crit_damage <- flat_damage * capped_crit_chance * crit_damage
  expected_damage_output <- (non_crit_damage + crit_damage) * attack_speed
  return(expected_damage_output)
}


comb_item_expected_damage <- function(item1, item2, item3, item4, item5, item6, item7) {
  # Convert string inputs to objects
  item1 <- get(item1)
  item2 <- get(item2)
  item3 <- get(item3)
  item4 <- get(item4)
  item5 <- get(item5)
  item6 <- get(item6)
  item7 <- get(item7)
  
  flat_damage <- flat_damage(item1$damage, item2$damage, item3$damage, item4$damage, item5$damage, item6$damage, item7$damage)
  crit_chance <- crit_chance(item1$crit_chance, item2$crit_chance, item3$crit_chance, item4$crit_chance, item5$crit_chance, item6$crit_chance, item7$crit_chance)
  crit_damage <- crit_damage(item1$crit_damage, item2$crit_damage, item3$crit_damage, item4$crit_damage, item5$crit_damage, item6$crit_damage, item7$crit_damage)
  attack_speed <- attack_speed(item1$attack_speed, item2$attack_speed, item3$attack_speed, item4$attack_speed, item5$attack_speed, item6$attack_speed, item7$attack_speed)
  
  expected_damage <- expected_damage(flat_damage, crit_chance, crit_damage, attack_speed)
  return(expected_damage)
  
}


#### ITEMS ####

# Define the constructor function for the 'item' class
item <- function(damage = 0, crit_chance = 0, crit_damage = 0, attack_speed = 0) {
  # Create a list with the properties
  obj <- list(
    damage = damage,
    crit_chance = crit_chance,
    crit_damage = crit_damage,
    attack_speed = attack_speed
  )
  # Set the class attribute to 'item'
  class(obj) <- "item"
  # Return the object
  return(obj)
}

null_item <- item() # optional if you want to see what not having full items looks like
chili_power <- item(attack_speed = 0.5)
sharp_axe <- item(damage = .66)
critical_damage_mace <- item(crit_damage = 0.75)
lucky_crit <- item(crit_chance = 0.5)
lethal_desperation <- item(damage = 0.125 * missing_health_multiplier)
mortal_fury <- item(attack_speed = 0.075 * missing_health_multiplier)


############################################################################################################################################
# CALCULATIONS
############################################################################################################################################

#### Create a table of all possible combinations ####

# Define the items
items <- c("chili_power", "sharp_axe", "critical_damage_mace", "lucky_crit", "lethal_desperation", "mortal_fury")

# Generate all combinations with replacement
all_combinations <- expand.grid(
  slot1 = items, 
  slot2 = items, 
  slot3 = items, 
  slot4 = items, 
  slot5 = items, 
  slot6 = items, 
  slot7 = items
)

# Sort each row and remove duplicates
unique_combinations <- unique(t(apply(all_combinations, 1, sort)))

# Convert back to a data frame
combos_df <- as.data.frame(unique_combinations)

names(combos_df) <- c("slot1", "slot2", "slot3", "slot4", "slot5", "slot6", "slot7")

# Calculated expected damage and sort on that
combos_df <- combos_df %>%
  rowwise() %>%
  mutate(expected_damage = comb_item_expected_damage(slot1, slot2, slot3, slot4, slot5, slot6, slot7)) %>%
  arrange(desc(expected_damage))


############################################################################################################################################
# VISUALIZATION
############################################################################################################################################

# Create a histogram of expected_damage 
ggplot(combos_df, aes(x = expected_damage)) +
  geom_histogram(binwidth = 100, # You can adjust the binwidth as needed
                 fill = "#21918c", 
                 color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Expected Damage for All Combinations of Grey Items",
       x = "Expected Damage",
       y = "Frequency") +
  theme_bw()




