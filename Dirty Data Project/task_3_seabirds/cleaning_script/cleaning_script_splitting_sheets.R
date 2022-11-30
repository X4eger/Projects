library(readr)
library(here)
library(readxl)
library(tidyverse)


# data is stored in one excel document on multiple sheets
# need to identify the different sheets we require and read them into
# seperate variables that we can clean

excel_sheets(here::here("raw_data/seabirds.xls"))

seabirds_ship_data <- read_excel(here::here("raw_data/seabirds.xls"))

seabirds_bird_data <- read_excel("raw_data/seabirds.xls", 
                                  sheet = "Bird data by record ID")

view(seabirds_bird_data)
view(seabirds_ship_data)

# Decided to save each sheet as an individual page for future cleaning and 
# to have a different cleaning script for each sheet

readr::write_csv(seabirds_bird_data, "raw_data/raw_bird_data.csv")
readr::write_csv(seabirds_ship_data, "raw_data/raw_ship_data.csv")
