library(readr)
library(dplyr)
library(stringr)
library(here)
library(lubridate)
library(googledrive)

#drive_deauth()
#drive_auth(scope = "https://www.googleapis.com/auth/drive")

# Step 1: Find the most recent date in the existing dataset
existing_data_path <- here::here("data/crime_data_raw.csv")
raw_data <- drive_upload(media = existing_data_path,
             name = "crime_data_raw.csv",
             path = as_id("1vlRyyJjao4mbf2MIHSLI81girSDGB4bm"))

# should see "shared=TRUE"
raw_data %>% 
  drive_reveal("permissions")

# Use the file ID directly to specify the file
file_id <- "1ZjEEKvB0CMBVvHOVh7gwXRQLcpPXo4r5"  # Replace with the actual file ID from the upload step
temp_file_path <- tempfile(fileext = ".csv")
drive_download(as_id(file_id), path = temp_file_path, overwrite = TRUE)
existing_data <- read_csv(temp_file_path)


existing_data <- readr::read_csv(existing_data_path) %>%
  #rename_with(~ str_to_lower(.) %>% stringr::str_replace_all(" ", "_")) %>%
  mutate(#`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
         zip = as.character(zip)) %>% 
#         state = "Arizona",
#         occurred_on = mdy_hm(occurred_on, truncated = 3),
#         occurred_to = mdy_hm(occurred_to, truncated = 3)) %>% 
  filter(!is.na(occurred_on))

# Convert "occurred_on" to Date type if it's not already
most_recent_datetime <- max((existing_data$occurred_on), na.rm = TRUE)

# Step 2: Download the new dataset
url <- "https://www.phoenixopendata.com/dataset/cc08aace-9ca9-467f-b6c1-f0879ab1a358/resource/0ce3411a-2fc6-4302-a33f-167f68608a20/download/crime-data_crime-data_crimestat.csv"
new_data <- read_csv(url) %>%
  rename_with(~ str_to_lower(.) %>% str_replace_all(" ", "_")) %>%
  mutate(`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
         zip = as.character(zip),
         state = "Arizona",
         occurred_on = mdy_hm(occurred_on, truncated = 3),
         occurred_to = mdy_hm(occurred_to, truncated = 3)) %>% 
  filter(!is.na(occurred_on))

new_entries <- new_data %>% 
  filter(new_data$occurred_on > most_recent_datetime)

# Step 4: Append new_entries to existing_data and save
if (nrow(new_entries) > 0) {
  updated_data <- bind_rows(existing_data, new_entries)
  write_csv(updated_data, existing_data_path)
  cat("New entries added:", nrow(new_entries), "\n")
} else {
  cat("No new entries to add. Dataset is up-to-date.\n")
}
