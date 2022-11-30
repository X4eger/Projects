

library(tidyverse)
library(janitor)
library(lubridate)

nhs_speciality <- read_csv(here::here("../raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_and_specialty.csv"))
nhs_age_sex <- read_csv(here::here("../raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_age_and_sex.csv"))
nhs_deprivation <- read_csv(here::here("../raw_data/inpatient_and_daycase_by_nhs_board_of_treatment_and_simd.csv"))
nhs_bed_occupancy <- read_csv("../raw_data/beds_by_nhs_board_of_treatment_and_specialty.csv")
covid_health_board <- read_csv(here::here("../raw_data/actual_covid_cases.csv"))
covid_cases_age_sex <- read_csv(here::here("../raw_data/trend_agesex_20221116.csv"))
covid_cases_deprivation <- read_csv(here::here("../raw_data/trend_simd_20221116.csv"))

# loading in codes - changing col names so they can be bound

nhs_hb_codes <- read_csv(here::here("../raw_data/health_board_codes.csv")) %>% 
  select(HB, HBName)
nhs_shb_codes <- read_csv(here::here("../raw_data/special_health_board_codes.csv")) %>% 
  select("HBName" = SHBName,
         "HB" = SHB) 
nhs_country_code <- read_csv(here::here("../raw_data/country_code.csv")) %>% 
  select("HBName" = CountryName,
         "HB" = Country)
nhs_other_code <- read_csv(here::here("../raw_data/other_codes.csv")) %>% 
  select("HBName" = ISDHBTName,
         "HB" = ISDHBT) 
nhs_hospital_code <- read_csv(here::here("../raw_data/hospital_codes.csv")) %>% 
  select("HBName" = LocationName,
         "HB" = Location)

# binding code tables together

codes <- bind_rows(nhs_hb_codes, nhs_shb_codes, nhs_country_code, nhs_other_code, nhs_hospital_code)


# data cleaning function - joins long names to the codes. reformats quarter col
clean_data_nhs <- function(data){
  
  data %>% 
    left_join(codes, by = c("HB")) %>% 
    left_join(codes, by = c("Location" = "HB")) %>% 
    select(1:2, HB, "health_board" = HBName.x, HBQF, "location_code" = Location, "location_name" = HBName.y,           everything()) %>% 
    clean_names() %>% 
    mutate(quarter = str_replace(quarter, pattern = "([0-9]{4})([Q][0-4])", replacement = "\\1 \\2"))
}

clean_data_covid <- function(data){
  
  data %>% 
    clean_names() %>% 
    mutate(date = ymd(date))
}

clean_data_covid_dep <- function(data){
  
  data %>% 
    left_join(codes, by = c("Country" = "HB")) %>% 
    select(1:2, Country, "country_name" = HBName, everything()) %>% 
    clean_names() %>% 
    mutate(date = ymd(date))
}

# write clean data
write_csv(nhs_speciality %>% clean_data_nhs(), here::here("../clean_data/nhs_speciality.csv"))
write_csv(nhs_deprivation %>% clean_data_nhs(), here::here("../clean_data/nhs_deprivation.csv"))
write_csv(nhs_age_sex %>% clean_data_nhs(), here::here("../clean_data/nhs_age_sex.csv"))
write_csv(nhs_bed_occupancy %>% clean_data_nhs(), here::here("../clean_data/nhs_bed_occupancy.csv"))
write_csv(covid_cases_age_sex %>% clean_data_covid_dep(), here::here("../clean_data/daily_covid_sex_age.csv"))
write_csv(covid_cases_deprivation %>% clean_data_covid_dep(), here::here("../clean_data/daily_covid_deprivation.csv"))
write_csv(covid_health_board %>% clean_data_covid(), here::here("../clean_data/daily_covid_health_board.csv"))

