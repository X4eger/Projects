library(readr)
library(here)
library(readxl)
library(tidyverse)
library(janitor)

candy_2015_raw <- read_excel(here::here("raw_data/boing-boing-candy-2015.xlsx"))
names(candy_2015_raw)

trim_2015_candy <- candy_2015_raw %>% 
  select(-(97:124)) # Remove all of the bizarre unhelpful questions at the end
names(trim_2015_candy)

trim_2015_candy <- clean_names(trim_2015_candy)

# Complete timestamp unnecessary, instead of removing we can modify to just be 
# the year, this will aid future analysis

trim_2015_candy <- trim_2015_candy %>% 
  mutate(timestamp = coalesce("2015")) %>% 
  rename(year = timestamp)

#renaming columns to make the bind easier

trim_2015_candy <- trim_2015_candy %>% 
  rename(age = how_old_are_you,
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself
  )

write_csv(trim_2015_candy, "clean_data/candy_2015.csv")
