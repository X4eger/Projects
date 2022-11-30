library(readr)
library(here)
library(readxl)
library(tidyverse)
library(janitor)

candy_2017_raw <- read_excel(here::here("raw_data/boing-boing-candy-2017.xlsx"))
names(candy_2017_raw)                             

trim_2017_candy <- candy_2017_raw %>% 
  select(-(110:120)) # Remove all of the bizarre unhelpful questions at the end
names(trim_2017_candy)

trim_2017_candy <- clean_names(trim_2017_candy)
# No timestamp column this year, will add for future use,there is no need
# for the internal ID column so will replace with year

trim_2017_candy <- trim_2017_candy %>% 
  mutate(internal_id = coalesce("2017")) %>% 
  rename(year = internal_id)

# looking at remaining columns, we want to try and remove all of the "q1-6_"

for( col in 1:ncol(trim_2017_candy)){
  
  colnames(trim_2017_candy) <- 
    sub("q[0-9]_", "", colnames(trim_2017_candy))
}
print(trim_2017_candy)

#renaming columns to make the bind easier

trim_2017_candy <- trim_2017_candy %>% 
  rename(
    sweetums = sweetums_a_friend_to_diabetes,
    state = state_province_county_etc,
    mary_janes = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes
  )

write_csv(trim_2017_candy, "clean_data/candy_2017.csv")
