library(tidyverse)
library(tidygeocoder)
library(readr)
library(stringr)
library(lubridate)
library(sf)
library(mapview)
library(here)
library(Hmisc)
options(scipen=999) # force full notation not scientific

# Paths
raw_data_path <- here::here("data/crime_data_raw.csv")
geolocated_data_path <- here::here("data/crime_data_geolocated.csv")

# Read raw data
raw_df <- read_csv(raw_data_path) %>%
  rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
  mutate(`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
         zip = as.character(zip),
         state = "Arizona",
         occurred_on = mdy_hm(occurred_on, truncated = 3),
         occurred_to = mdy_hm(occurred_to, truncated = 3),
         lat = NA_real_,
         long = NA_real_) %>%  # Handle dates with potential missing time parts
  filter(!is.na(occurred_on))


# Geolocated data
if (file.exists(geolocated_data_path)) {
  geo_df <- read_csv(geolocated_data_path) %>% 
    mutate(zip = as.character(zip))
} else {
  # Initialize geolocated data from raw data
  geo_df <- read_csv(raw_data_path) %>%
    rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
    mutate(`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
           zip = as.character(zip),
           state = "Arizona",
           occurred_on = mdy_hm(occurred_on, truncated = 3),
           occurred_to = mdy_hm(occurred_to, truncated = 3)) %>%
    filter(!is.na(occurred_on)) %>%
    #select(-lat, -long) %>%  # Remove lat and long if they exist
#    mutate(lat = NA_real_, long = NA_real_) %>%
    slice(0)  # Select zero rows to keep just the column structure
  
  write_csv(geo_df, geolocated_data_path)
}

records_to_process <- anti_join(raw_df, geo_df, by = c("occurred_on", "100_block_addr", "zip"))

# Geoprocessing
geo_df <- records_to_process %>%
  slice(1:100) %>% # throttling for now for testing
  tidygeocoder::geocode(
    street = "100_block_addr",
    postalcode = "zip",
    state = "state",
    method = "osm"
)
write_csv(geo_df, geolocated_data_path)


geo_sf <- geo_df %>%
  sf::st_as_sf(
    coords = c("long", "lat"),
    crs = 4326)

# Mapping
geo_sf %>% mapview()

