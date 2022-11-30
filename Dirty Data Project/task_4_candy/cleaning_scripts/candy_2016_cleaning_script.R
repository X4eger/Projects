library(readr)
library(here)
library(readxl)
library(tidyverse)
library(janitor)

candy_2016_raw <- read_excel(here::here("raw_data/boing-boing-candy-2016.xlsx"))
names(candy_2016_raw)                             

trim_2016_candy <- candy_2016_raw %>% 
  select(-(107:123)) # Remove all of the bizarre unhelpful questions at the end
names(trim_2016_candy)

trim_2016_candy <- clean_names(trim_2016_candy)
# Complete timestamp unnecessary, instead of removing we can modify to just be 
# the year, this will aid future analysis

trim_2016_candy <- trim_2016_candy %>% 
  mutate(timestamp = coalesce("2016")) %>% 
  rename(year = timestamp)

#renaming columns to make the bind easier

trim_2016_candy <- trim_2016_candy %>% 
  rename(age = how_old_are_you,
         gender = your_gender,
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         country = which_country_do_you_live_in,
         state = which_state_province_county_do_you_live_in,
         sweetums = sweetums_a_friend_to_diabetes
         )


write_csv(trim_2016_candy, "clean_data/candy_2016.csv")
