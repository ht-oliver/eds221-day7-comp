# Clear environment
rm(list = ls())

# Attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate)
library(dplyr)

# Data wrangling refresher

# 1) only include penguins at Brisco and Dream Islands
# 2) Remove year and sex variables 
# 3) Add a new column called body mass in kg with penguin mass converted from grams to kilograms
# 4) Rename island variable to location

penguins %>%
  filter(island %in% c("Biscoe", "Dream")) %>% 
  select(-year, -sex) %>% 
  mutate("mass_kg" = body_mass_g/1000) %>% 
  rename(location = island)

# 1. Limit to only Adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, std, and sample size (n()) of flipper lengths for male and females
penguins %>% 
  filter(species == "Adelie") %>%
  filter(!is.na(flipper_length_mm),
         !is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n())
penguins


animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)


sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

# practice with a full_join

full_join(animals, sites)

left_join(animals, sites)            

right_join(sites, animals)  

inner_join(animals, sites)


# Now filtering joins

# these two are the same
semi_join(animals, sites)

animals %>% 
  filter(location %in% sites$location)


# these two are the same
anti_join(animals, sites)  

animals %>% 
  filter(!location$ sites$location)


