library(readr)
library(tidyverse)
library(janitor)

# Read raw ship data in

raw_ship_data <- read_csv("raw_data/raw_ship_data.csv")
view(raw_ship_data)

# Having already looked at the questions and what we may need to answer, 
# we should look to trim the ship data down to include: 
# ID, RECORD ID, LAT, LONG
raw_ship_data <- clean_names(raw_ship_data)

names(raw_ship_data)

trim_ship_data <- raw_ship_data %>% 
  select(record, record_id, lat, long)

head(trim_ship_data, 10)

# After trimming the data down to useful columns, should check for NA's

trim_ship_data %>% 
  summarise(across(.fns = ~sum(is.na(.))))

trim_ship_data %>% 
  filter(is.na(trim_ship_data$long))

clean_ship_data <- trim_ship_data
# writing out the ship data as is to see how it joins with bird data

readr::write_csv(clean_ship_data, "clean_data/clean_ship_data.csv")


