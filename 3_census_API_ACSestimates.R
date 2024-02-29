library(tidycensus)
library(tigris)
library(sf)
library(here)
library(dplyr)

api_key_file <- "/Users/natebender/Desktop/repo/census_2024_api_key.txt"
census_api_key <- readLines(api_key_file)
census_geolocated_path <- here::here("data/census_geolocated.csv")

# Load the crime data
crime_data <- read.csv(census_geolocated_path)
crime_data <- crime_data %>% 
  filter(!is.na(lat),
         !is.na(long),
         unique_ID != "unique_ID")

crime_data_sf <- st_as_sf(crime_data, coords = c("long", "lat"), crs = 4326, agr = "constant")
crime_data_sf <- st_set_geometry(crime_data_sf, "incident_geom")

# 013 for Maricopa County and 021 for Pinal County
block_groups <- block_groups(state = "AZ", county = c("013", "021"), class = "sf") %>%
  st_transform(., crs = st_crs(crime_data_sf))

# Perform the spatial join to append block group identifiers to crime data
crime_with_bg <- st_join(crime_data_sf, block_groups)

bg_geometries <- block_groups %>% 
  select(GEOID, geometry)

# First, convert the block group geometries to a non-sf data frame for the merge
bg_geometries_df <- as.data.frame(bg_geometries)

# Ensure there's a geometry column in bg_geometries_df that contains the geometry as a list or WKT (Well-Known Text)
bg_geometries_df$geometry <- st_as_text(bg_geometries$geometry)

# Now, merge using dplyr's left_join, which should work since we're no longer using an sf object directly
crime_with_bg <- crime_with_bg %>%
  left_join(bg_geometries_df, by = "GEOID")

# Convert the text geometries back to sf geometries and set as the active geometry column
crime_with_bg <- st_as_sf(crime_with_bg, wkt = "geometry", crs = st_crs(crime_with_bg))
crime_with_bg <- st_set_geometry(crime_with_bg, "geometry")


