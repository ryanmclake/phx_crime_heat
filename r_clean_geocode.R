library(tidyverse)
library(tidygeocoder)
library(readr)
library(stringr)
library(lubridate)
library(sf)
library(mapview)
library(here)
library(Hmisc)
library(scales)
library(hms)
options(scipen=999) # force full notation not scientific

start_time <- Sys.time()  # Capture start time

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
         occurred_to = mdy_hm(occurred_to, truncated = 3)) %>% 
  filter(!is.na(occurred_on))

# Read geolocated data
if (file.exists(geolocated_data_path)) {
  geo_df <- read_csv(geolocated_data_path) %>% 
    mutate(zip = as.character(zip))
} else {
  # Initialize colnames and structure from raw data
  geo_df <- read_csv(raw_data_path) %>%
    rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
    mutate(`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
           zip = as.character(zip),
           state = "Arizona",
           occurred_on = mdy_hm(occurred_on, truncated = 3),
           occurred_to = mdy_hm(occurred_to, truncated = 3)) %>%
    filter(!is.na(occurred_on)) %>%
    slice(0)  # Select zero rows to keep just the column structure
  write_csv(geo_df, geolocated_data_path)
}

# Geoprocessing
geoprocess_data <- function(raw_df, geo_df, geolocated_data_path) {

  records_to_process <- anti_join(raw_df, geo_df, by = c("occurred_on", "100_block_addr", "zip"))
  
  # Geocode the records to process
  geo_processed <- records_to_process %>%
    sample_n(500, replace = FALSE) %>%  # Throttle for testing
    tidygeocoder::geocode(
      street = "100_block_addr",
      postalcode = "zip",
      state = "state",
      method = "osm"
    )
  
  sec_remain <- nrow(records_to_process)
  hr_remain <- sec_remain %/% 3600
  min_remain <- (sec_remain %% 3600) %/% 60
  
  cat("Processed", nrow(geo_processed), "records\n")
  cat("There are", scales::comma(nrow(records_to_process)), "records remaining\n")
  cat(sprintf("Estimated processing remaining: %02d hours and %02d minutes\n", hr_remain, min_remain))
  
  # Write the geocoded data to file
  write_csv(geo_processed, geolocated_data_path, append = T)
  
  end_time <- Sys.time()  # Capture end time
  duration <- end_time - start_time  # Calculate duration
  total_sec <- as.numeric(duration)
  hours <- total_sec %/% 3600
  minutes <- (total_sec %% 3600) %/% 60
  seconds <- total_sec %% 60
  formatted_duration <- sprintf("%02d:%02d:%02d", hours, minutes, round(seconds))
  cat("Processing took:", formatted_duration, "h/m/s\n")
  
  }

geoprocess_data(raw_df, geo_df, geolocated_data_path)





