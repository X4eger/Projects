library(readr)
library(tidyverse)
library(janitor)

# Read bird data back in
raw_bird_data <- read_csv("raw_data/raw_bird_data.csv")
view(raw_bird_data)

# A lot of information, in order to identify what we should keep and
# what we can potentially remove, check what we need to find out from the
# questions around the data, these are two massive tables, so if we can remove
# some data before joining it will make it a bit easier for the next stage


# Do we need all the variables for this data?
#   You’ll need to join the ship data to the bird record data
        # Looking at both datasets and the dictionary attached we should be
        # joining these by RECORD ID

# 1.3.2 Analysis questions

# For each question where we ask for bird names, give the bird’s common name, 
# scientific name and species abbreviation.
      # Make sure we keep these 3 columns

#  Which bird had the most individual sightings?

#  Which bird had the highest total count?

#  Which bird had the highest total count above a latitude of -30?
    # This Question shows we are going to need to attach the ship data to 
    # get latitude before filtering

#  How many different types of birds were only ever seen in groups of 1?
      # Birdcount can be useful from the ship data

#  How many penguins were seen? (Hint: there are many types of penguin)

raw_bird_data <- clean_names(raw_bird_data)

# rename the name columns to be shorter and more concise

raw_bird_data <- raw_bird_data %>% 
  rename(
    species_common_name = species_common_name_taxon_age_sex_plumage_phase,
    species_scientific_name = species_scientific_name_taxon_age_sex_plumage_phase
  )

names(raw_bird_data)
#identify which columns aren't required for answering any of the questions: 
    # we want to keep the following:
    # record, record_id, species_common_name, species_scientific_name, 
    # species_abbreviation, count

trim_bird_data <- raw_bird_data %>% 
  select(record, record_id, species_common_name, species_scientific_name, 
         species_abbreviation, count) 

head(trim_bird_data, 10)
##Check for NA entries
trim_bird_data %>% 
  summarise(across(.fns = ~sum(is.na(.))))

# scientific name, count have NA values
# we can change the count to 0 instead of NA and then check
# what entries have NA for scientific name and why

trim_bird_data$count[is.na(trim_bird_data$count)]<- 0

# Now to identify which rows have NA in scientific name and why

trim_bird_data %>% 
  filter(is.na(trim_bird_data$species_scientific_name))
# [No BIRDS RECORDED] is an entry that should be removed as this doesn't help us
# Going to check how many entries have this before removing it

trim_bird_data %>% 
  filter(species_common_name == "[NO BIRDS RECORDED]") # 681 rows of no birds

trim_bird_data <- trim_bird_data %>% 
  filter(species_common_name != "[NO BIRDS RECORDED]")

# With the majority of NA's removed now to identify what is left and decide if 
# it is worth keeping
trim_bird_data %>% 
  filter(is.na(trim_bird_data$species_scientific_name)) %>% 
  distinct(species_common_name)

# As all of the remaining NA columns are unidentified birds with common names,
# remaining options will be changed to "Unidentified"

clean_bird_data <- trim_bird_data %>% 
  mutate(species_scientific_name = 
           coalesce(species_scientific_name, "Unidentified"))

# analysis of data shows that entries with a count of 0 should be removed prior
# to any further work
clean_bird_data <- clean_bird_data %>% 
  filter(trim_bird_data$count != 0)


# Write out the cleaned data

  readr::write_csv(clean_bird_data, "clean_data/clean_bird_data.csv")
