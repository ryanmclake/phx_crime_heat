library(readr)
library(dplyr)
library(stringr)
library(here)
library(lubridate)

# Step 1: Find the most recent date in the existing dataset
existing_data_path <- here::here("data/crime_data_raw.csv")
existing_data <- readr::read_csv(existing_data_path) %>%
  rename_with(~ str_to_lower(.) %>% stringr::str_replace_all(" ", "_")) %>%
  mutate(`100_block_addr` = str_replace_all(`100_block_addr`, "XX", "00"),
         zip = as.character(zip),
         state = "Arizona",
         occurred_on = mdy_hm(occurred_on, truncated = 3),
         occurred_to = mdy_hm(occurred_to, truncated = 3)) %>% 
  filter(!is.na(occurred_on))

# Assuming "occurred_on" is in the format "YYYY-MM-DD"
# Convert "occurred_on" to Date type if it's not already
most_recent_date <- max(as.Date(existing_data$occurred_on), na.rm = TRUE)

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
  mutate(date_filter = as.Date(new_data$occurred_on)) %>% 
  filter(date_filter > most_recent_date) %>% 
  select(-date_filter)

# Step 4: Append new_entries to existing_data and save
if (nrow(new_entries) > 0) {
  updated_data <- bind_rows(existing_data, new_entries)
  write_csv(updated_data, existing_data_path)
  cat("New entries added:", nrow(new_entries), "\n")
} else {
  cat("No new entries to add. Dataset is up-to-date.\n")
}
