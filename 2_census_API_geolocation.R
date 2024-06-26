library(googledrive)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(Hmisc)
library(lubridate)
library(here)
library(httr) # census api

# Script for geolocating lat/long coordinates to the Phoenix Police crime data.

# Ignore these two steps with the initial raw data if you don't need to upload any geolocated data to GDrive initially
# census_geolocated_path <- here::here("data/census_geolocated.csv")
# raw_data_geolocated <- drive_upload(media = census_geolocated_path,
#              name = "census_geolocated.csv",
#              path = as_id("1vlRyyJjao4mbf2MIHSLI81girSDGB4bm")) # folder id 
# # should see "shared=TRUE"
# raw_data_geolocated %>%
#    drive_reveal("permissions")

start_time <- Sys.time()  # Capture start time

# Paths
census_forgeolocation_path <- here::here("data/census_forgeolocation.csv")
census_temp_path <- here::here("data/census_temp.csv")

# Read existing geolocated data
geolocated_path <- drive_find(pattern = "census_geolocated.csv")
file_id <- geolocated_path$id  # this takes a min or two
temp_file_path <- tempfile(fileext = ".csv")
drive_download(as_id(file_id), path = temp_file_path, overwrite = TRUE)
df_geolocated <- read_csv(temp_file_path) %>%
  rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
  mutate(zip = as.character(zip),
         lat = as.double(lat),
         long = as.double(long)
         )

# Download existing raw data from Google Drive
# this usually takes a min or two
raw_data_path <- drive_find(pattern = "crime_data_raw.csv")
file_id <- raw_data_path$id  # Replace with the actual file ID from the upload step
temp_file_path <- tempfile(fileext = ".csv")
drive_download(as_id(file_id), path = temp_file_path, overwrite = TRUE)

df_raw <- read_csv(temp_file_path) %>%
  mutate(zip = as.character(zip)) %>%
  filter(!is.na(occurred_on))

# raw_df <- read_csv(raw_data_path) %>%
#   rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
#   mutate(zip = as.character(zip)) %>% 
#   filter(!is.na(occurred_on))



# old - Read geolocated data
# if (file.exists(census_geolocated_path)) {
#   geo_df <- read_csv(census_geolocated_path) %>% 
#     mutate(zip = as.character(zip))
# } else {
#   # Initialize colnames and structure from raw data
#   geo_df <- df_raw %>% 
#     slice(0)  # Select zero rows to keep just the column structure
#   write_csv(geo_df, census_geolocated_path)
# }

# Grab just the unique combos of addr and zip
df_raw <- df_raw %>%
  distinct(`100_block_addr`, `zip`, .keep_all = T) # .keep_all keeps all cols in place

temp_records_to_process <- anti_join(df_raw, df_geolocated, by = c("100_block_addr", "zip"))

records_to_process <- head(temp_records_to_process, 9000)

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
    writeLines(
    httr::content(response, "text"), 
    census_temp_path)
  cat("Geocoding results saved to 'census_temp_path'.\n")
} else {
  cat("Failed to submit batch geocoding request. Status code:", response$status_code, "\n")
}

api_response <- read_csv(census_temp_path, col_names = F)

# Sometimes the API spits out the data using 8 columns. Other times, for no apparent reason, it condenses the data
# into 3 columns. I can't figure out what drives this behavior, so I just wrote code to deal with both scenarios. 

if (length(api_response) <= 3) {
  col_names <- c("unique_ID", "input_address", "match_status")
  api_response <- read_csv(census_temp_path, col_names = col_names)
  
  api_response <- api_response %>% 
    mutate(match_status_type = case_when(
      grepl("^Match", match_status) ~ "match",
      grepl("^No_Match", match_status) | grepl("^Tie", match_status) ~ "no_match_or_tie",
      TRUE ~ "unknown"  # Handles any unexpected cases as "unknown"
    ))
  
  no_matches_ties <- api_response %>%
    filter(match_status_type == "no_match_or_tie",
           unique_ID != "Unique ID") %>% 
    separate(input_address, into = c("100_block_addr", "city", "state", "zip"), sep = ", ", fill = "right") %>% 
    mutate(long = NA_real_,
           lat = NA_real_) %>% 
    select(-c("city", "match_status_type"))
  
  matches <- api_response %>%
    separate(input_address, into = c("100_block_addr", "city", "state", "zip"), sep = ", ", fill = "right") %>% 
    select(-city) %>%
    filter(match_status_type == "match") %>%
    separate(match_status, into = c("match_status", "match_type", "output_address",
                                    "output_city", "output_state", "output_zip", "long", "lat",
                                    "tiger_id", "side"), sep = ",", fill = "right", extra = "merge") %>% 
    # drop unnecessary cols  
    select(-c("match_status_type",  "match_type", "output_address",
                "output_city", "output_state", "output_zip",
                "tiger_id", "side")) %>% 
    mutate(long = as.double(long),
           lat = as.double(lat)) %>% 
    group_by(long, lat) %>%
    # Ensure all addresses within each lat/long pair are the same 
    # by replacing them with the first address encountered in each pair
    # There are occasional discrepancies in the addresses e.g. "St" v "Ave"
    mutate(`100_block_addr` = first(`100_block_addr`))
  
  # bind "matches" and "no_matches_ties" dataframes together after processing
  # Remove duplicate addresses as determined by address/zip pairings
  api_response_df <- bind_rows(matches, no_matches_ties) %>% 
    group_by(`100_block_addr`, zip) %>%
    slice(1) %>%
    ungroup()
  
  #write_csv(api_response_df, census_geolocated_path, col_names = T, append = T)

  df_geolocated_upd <- df_geolocated %>%
    bind_rows(api_response_df)
  write_csv(df_geolocated_upd, temp_file_path, col_names = TRUE)
  drive_update(as_id(file_id), media = temp_file_path)
  
} else {
  col_names <- c("unique_ID", "input_address", "match_status", "match_type", "output_address", "coordinates", "tiger_id", "side")
  api_response <- read_csv(census_temp_path, col_names = col_names)
  
  api_response <- api_response %>% 
    mutate(match_status_type = case_when(
      grepl("^Match", match_status) ~ "match",
      grepl("^No_Match", match_status) | grepl("^Tie", match_status) ~ "no_match_or_tie",
      TRUE ~ "unknown"  # Handles any unexpected cases as "unknown"
    )) %>% 
    separate(coordinates, into = c("long", "lat"), sep = ",", convert = T)

  no_matches_ties <- api_response %>%
    filter(match_status_type == "no_match_or_tie",
           unique_ID != "Unique ID") %>% 
    separate(input_address, into = c("100_block_addr", "city", "state", "zip"), sep = ", ", fill = "right") %>% 
    mutate(long = NA_real_,
           lat = NA_real_) %>% 
    select(-c("city", "match_type", "match_status_type", "output_address", "tiger_id", "side"))
  
  matches <- api_response %>%
    separate(input_address, into = c("100_block_addr", "city", "state", "zip"), sep = ", ", fill = "right") %>% 
    select(-city) %>%
    filter(match_status_type == "match") %>%
    # drop unnecessary cols  
    select(-c("match_status_type",  "match_type", "output_address",
              "tiger_id", "side")) %>% 
    mutate(long = as.double(long),
           lat = as.double(lat)) %>% 
    group_by(long, lat) %>%
    # Ensure all addresses within each lat/long pair are the same 
    # by replacing them with the first address encountered in each pair
    # There are occasional discrepancies in the addresses e.g. "87 Example St" v "87 Example Ave"
    mutate(`100_block_addr` = first(`100_block_addr`))
  
  # bind "matches" and "no_matches_ties" dataframes together after processing
  # Remove duplicate addresses as determined by address/zip pairings
  api_response_df <- bind_rows(matches, no_matches_ties) %>% 
    group_by(`100_block_addr`, zip) %>%
    slice(1) %>%
    ungroup()
  
  api_response_df <- api_response_df %>% 
    rename(block_addr_100 = `100_block_addr`)
  
#  write_csv(api_response_df, census_geolocated_path, col_names = T, append = T)
  
  df_geolocated_upd <- df_geolocated %>%
    bind_rows(api_response_df)
  write_csv(df_geolocated_upd, temp_file_path, col_names = TRUE)
  drive_update(as_id(file_id), media = temp_file_path)
  
}

# delete temp files created during the api process
file.remove(here("data", "census_temp.csv"))
file.remove(here("data", "census_forgeolocation.csv"))

end_time <- Sys.time()  # Capture end time
duration_sec <- as.numeric(end_time - start_time, units = "secs")  # Calculate duration
hours <- duration_sec %/% 3600
minutes <- (duration_sec %% 3600) %/% 60
seconds <- duration_sec %% 60
formatted_duration <- sprintf("%02d:%02d:%02d", hours, minutes, round(seconds))

cat("Records processed on this run:", scales::comma(nrow(api_response_df)), "\n")
cat("Records processed in total:", scales::comma(nrow(df_geolocated)+nrow(api_response_df)), "\n")
cat("Records remaining:", scales::comma(nrow(temp_records_to_process)-nrow(api_response_df)), "\n")
cat("Time elapsed:", formatted_duration, "h/m/s\n")

