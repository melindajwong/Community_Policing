# Public Interest Data::Ethics & Practice
# Race Community Policing Data
# Packages: sf, tigris, leaflet
# 2023-03-30
# Melinda Wong

# ..................................................
# Set up ----

# Load libraries
library(tidyverse) 
library(janitor)
library(lubridate)
library(tidycensus)
library(dplyr)
library(tigris)

# ..................................................
# Read data ----

## Statewide police stops ----
url <- "https://data.virginia.gov/api/views/2c96-texw/rows.csv?accessType=DOWNLOAD"
va_stops <- read_csv(url)

va_stops <- va_stops %>% 
  clean_names() %>% 
  separate(incident_date, into = c("date1", "time", "ampm"), sep = " ", remove = FALSE) %>% 
  mutate(date = mdy(date1),
         year = year(date))

retained_variables <- c('jurisdiction', 'race','reason_for_stop', 'action_taken', 'residency', 'year')

stops_race <- va_stops %>% 
  clean_names() %>% 
  group_by(across(all_of(retained_variables))) %>% 
  summarise(total_count=n())
stops_race$residency[stops_race$residency == "RESIDENT OF TOWN/CITY/COUNTY OF STOP"] <- "RESIDENT OF JURISDICTION OF STOP"
stops_race$race[stops_race$race == "ASIAN/PACIFIC ISLANDER"] <- "ASIAN OR PACIFIC ISLANDER"
stops_race$action_taken[stops_race$action_taken == "CITATION/SUMMONS"] <- "CITATION OR SUMMONS"


stops_race_fixed <- stops_race %>% 
  filter(jurisdiction != "MISSING") %>% 
  mutate(locality = str_to_title(jurisdiction),
         locality = str_replace(locality, " Co", " County"),
         locality = ifelse(str_detect(locality, "City|County"),
                           locality, 
                           paste(locality, "city")),
         locality = str_replace(locality, "City", "city"),
         locality = str_replace(locality, "Countyunty", "County"),
         locality = str_replace(locality, "city County", "City County")) %>% 
  select(jurisdiction, locality, everything()) 



## Locality race data
decc_vars <- load_variables(2020, "pl", cache = TRUE)

# All variables for one race and two or more races
vars <- c(WHITE = "P1_003N",
          BLACK_OR_AFRICAN_AMERICAN = "P1_004N",
          AMERICAN_INDIAN = "P1_005N",
          ASIAN = "P1_006N",
          NATIVE_HAWAIIAN = "P1_007N")

# white_black = "P1_011N",
# white_american_indian = "P1_012N",
# white_asian = "P1_013N",
# white_native_hawaiian = "P1_014N",
# black_american_indian = "P1_016N",
# black_asian = "P1_017N",
# black_native_hawaiian = "P1_018N",
# american_indian_asian = "P1_020N",
# american_indian_native_hawaiian = "P1_021N",
# asian_native_hawaiian = "P1_023N"

# Retrieves 2020 census data from all countied in Virginia
race <- get_decennial(geography = "county", 
              var = vars, 
              year = 2020,
              state = "VA",
              output = "wide")

race_per <- race %>% 
  adorn_percentages() %>% 
  mutate(ASIAN_OR_PACIFIC_ISLANDER = ASIAN+NATIVE_HAWAIIAN) %>% 
  select(!c(ASIAN, NATIVE_HAWAIIAN))

colnames(race_per) <- c("GEOID", "NAME", "WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN", "ASIAN OR PACIFIC ISLANDER")

  
## Geography Data
va_counties <- counties(state = "51", cb = TRUE)
va_counties$GEOID <- as.numeric(va_counties$GEOID)

# ..................................................
# Export Data ----      
write_csv(race_per, "data/race_per.csv")
write_csv(stops_race_fixed, "data/stops.csv")
write_csv(va_counties, "data/va_counties.csv")
