#### Experimenting with Census geocoding API rather than Nominatim #######################################
# **********************************************************************************
library(dplyr)
library(httr)
library(readr)
library(stringr)
library(tidyr)
library(Hmisc)
library(lubridate)

start_time <- Sys.time()  # Capture start time

# Paths
raw_data_path <- here::here("data/crime_data_raw.csv")
geolocated_data_path <- here::here("data/crime_data_geolocated.csv")
census_forgeolocation_path <- here::here("data/census_forgeolocation.csv")
census_temp_path <- here::here("data/census_temp.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv")

# Read raw data
raw_df <- read_csv(raw_data_path) %>%
  rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
  mutate(zip = as.character(zip)) %>% 
  filter(!is.na(occurred_on))

# Read geolocated data
if (file.exists(census_geolocated_path)) {
  geo_df <- read_csv(census_geolocated_path) %>% 
    mutate(zip = as.character(zip),
           occurred_on = as.POSIXct(occurred_on, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
} else {
  # Initialize colnames and structure from raw data
  geo_df <- read_csv(raw_data_path) %>%
    rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
    mutate(`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
           zip = as.character(zip),
           state = "Arizona",
           occurred_on = lubridate::mdy_hm(occurred_on, truncated = 3),
           occurred_to = lubridate::mdy_hm(occurred_to, truncated = 3)) %>%
    filter(!is.na(occurred_on)) %>%
    slice(0)  # Select zero rows to keep just the column structure
  write_csv(geo_df, census_geolocated_path)
}

# Prepare the data frame as before
records_to_process <- anti_join(raw_df, geo_df, by = c("occurred_on", "100_block_addr", "zip"))

records_to_process <- head(records_to_process, 100)

# Add a unique ID and select necessary columns (adjust column names as necessary)
records_to_process <- records_to_process %>%
  mutate(unique_id = row_number()) %>%
  select(unique_id, `100_block_addr`, state, zip) # Adjust 'city_name' as needed

# Add a unique ID, a placeholder "City" column with NULL values, and select necessary columns
records_to_process <- records_to_process %>%
  mutate(`Unique ID` = row_number(),
         `Street address` = `100_block_addr`,
         `City` = NA, # Add placeholder "City" column with null values
         `State` = state,
         `ZIP` = zip) %>%
  select(`Unique ID`, `Street address`, `City`, `State`, `ZIP`)

# Export to CSV, ensuring column names match the API's expected format
write.csv(records_to_process, census_forgeolocation_path, row.names = FALSE, quote = FALSE)

# Use httr to perform the API submission
api_url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

# Prepare the file and form data for the request
file_to_upload <- httr::upload_file(census_forgeolocation_path)
form_data <- list(
  addressFile = file_to_upload,
  benchmark = "Public_AR_Current" # Adjust benchmark as necessary
)

# Perform the POST request
response <- httr::POST(url = api_url, body = form_data, encode = "multipart")

if (response$status_code == 200) {
  # Extract the content as text

  # Save the content to a file
  writeLines(
    httr::content(response, "text"), 
    census_temp_path)
  cat("Geocoding results saved to 'census_temp_path'.\n")
} else {
  cat("Failed to submit batch geocoding request. Status code:", response$status_code, "\n")
}

#test <- read_csv(census_temp_path)
test2 <- read_csv(census_temp_path)


col_names <- c("Unique_ID", "Input_Address", "Match_Status", "Match_Type", "Output_Address", "Coordinates", "tiger_id", "side")
# Read the CSV file with specified column names
census_geodf <- read_csv(census_temp_path, col_names = col_names)
census_geodf <- census_geodf %>%
  separate(Coordinates, into = c("long", "lat"), sep = ",", convert = TRUE) %>% 
  separate(Input_Address, into = c("100_block_addr", "city", "state", "zip"), sep = ",", convert = TRUE) %>% 
  filter(Unique_ID != "Unique ID") %>% 
  mutate(zip = str_trim(zip, side = "left")) %>% 
  select(`100_block_addr`, zip, long, lat)

merged_df <- raw_df %>%
  left_join(census_geodf, by = c("100_block_addr", "zip")) %>% 
  filter(!is.na(lat) & !is.na(long))


write_csv(merged_df, census_geolocated_path, col_names = T, append = T)

end_time <- Sys.time()  # Capture end time
duration_sec <- as.numeric(end_time - start_time, units = "secs")  # Calculate duration
hours <- duration_sec %/% 3600
minutes <- (duration_sec %% 3600) %/% 60
seconds <- duration_sec %% 60
formatted_duration <- sprintf("%02d:%02d:%02d", hours, minutes, round(seconds))
cat("Time elapsed:", formatted_duration, "h/m/s\n")

