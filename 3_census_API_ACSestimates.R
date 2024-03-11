library(tidycensus)
library(tidyr)
library(readr)
library(tigris)
library(ggplot2)
library(mapview)
library(sf)
library(here)
library(dplyr)
library(viridis)
start_time <- Sys.time()  # Capture start time

api_key_file <- "/Users/natebender/Desktop/repo/census_2024_api_key.txt"
census_api_key <- readLines(api_key_file)
raw_data_path <- here::here("data/crime_data_raw.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv")
df_complete_path <- here::here("data/df_complete_forapp.gpkg")
blockgroups_geom_path <- here::here("data/blockgroups_geom.gpkg")

calc_duration <- function(start_time, end_time) {
  duration_sec <- as.numeric(end_time - start_time, units = "secs")
  hours <- duration_sec %/% 3600
  minutes <- (duration_sec %% 3600) %/% 60
  seconds <- duration_sec %% 60
  formatted_duration <- sprintf("%02d:%02d:%02d", hours, minutes, round(seconds))
  return(formatted_duration)
}


raw_df <- readr::read_csv(raw_data_path) %>% 
  mutate(zip = as.character(zip)) %>% 
  rename(block_addr_100 = `100_block_addr`)

geo_df <- read_csv(census_geolocated_path) %>%
  rename(block_addr_100 = `100_block_addr`) %>% 
  filter(!is.na(lat),!is.na(long),
         block_addr_100 != "100_block_addr") %>%
  mutate(lat = as.numeric(lat),
          long = as.numeric(long)) %>%  
  select(c("block_addr_100", "zip", "lat", "long"))

# show duplicates
# geo_df %>%
#   add_count(block_addr_100, zip, name = "n") %>%
#   filter(n > 1) %>%
#   arrange(block_addr_100, zip) %>%
#   select(block_addr_100, zip, lat, long, n)

geo_df <- geo_df %>%
  distinct(block_addr_100, zip, .keep_all = TRUE)

acs_df <- raw_df %>% 
  left_join(geo_df, by = c("block_addr_100", "zip")) %>% 
  filter(!is.na(lat),!is.na(long))# %>% 
  # filter for now until performance is optimized
#  slice_sample(n=500, replace = F)

# Load the crime data
# crime_data <- read.csv(census_geolocated_path)
# crime_data <- crime_data %>% 
#   filter(!is.na(lat),
#          !is.na(long),
#          unique_ID != "unique_ID") %>% 
#   rename(block_addr_100 = X100_block_addr)

#### Spatial setup ####
acs_sf <- st_as_sf(acs_df, coords = c("long", "lat"), crs = 4326, agr = "constant")
acs_sf <- st_set_geometry(acs_sf, "geometry")

# 013 for Maricopa County and 021 for Pinal County
block_groups <- block_groups(state = "AZ", county = c("013", "021"), class = "sf") %>%
  st_transform(., crs = st_crs(acs_sf))

block_groups <- block_groups %>% 
  rename(bg_geoid = GEOID) %>% 
  select(bg_geoid, geometry)

#### ACS data ####
# Specify the variables of interest
variables <- c(median_income = "B19013_001", total_pop = "B02001_001", white_alone = "B02001_002")

# Download ACS data for block groups in Maricopa County (013) and Pinal County (021) in Arizona
acs_response <- get_acs(key = census_api_key,
                    geography = "block group",
                    variables = variables,
                    state = "AZ",
                    county = c("013", "021"),
                    survey = "acs5",  # Specify the survey; acs5 for 5-year estimates
                    year = 2022,  # Specify the most recent year you're interested in
                    output = "wide")  # Wide format for easier handling

# Transform the data to calculate % people of color
acs_data <- acs_response %>%
  mutate(percent_poc = 100 * (total_popE - white_aloneE) / total_popE) %>%
  rename(bg_geoid = GEOID) %>% 
  select(bg_geoid, median_incomeE, percent_poc)

# Merge the ACS data with your block groups spatial data
block_groups <- block_groups %>%
  left_join(acs_data, by = c("bg_geoid" = "bg_geoid"))

### Phoenix Police jurisdiction polygon
url <- "https://mapping-phoenix.opendata.arcgis.com/api/download/v1/items/811dd8a9568d4d018fc9a6f6ef8ed329/shapefile?layers=0"
zip_dest <- here::here("data/phoenix_shapefile.zip")
download.file(url, zip_dest, mode = "wb")
unzip(zip_dest, exdir = "data/phoenix_shapefile")
shapefile_path <- "data/phoenix_shapefile" # Adjust this path to the actual unzipped shapefile location
phoenix_pd <- st_read(shapefile_path) %>% 
  st_transform(st_crs(block_groups)) # Ensure CRS match

block_groups <- block_groups %>% 
  filter(st_intersects(geometry, st_geometry(phoenix_pd), sparse = FALSE) %>% apply(1, any))
  
acs_sf_blockgroups <- block_groups %>% 
  st_join(acs_sf)



# Perform the spatial join to append block group identifiers to crime incidents
#acs_sf_blockgroups <- st_join(acs_sf, block_groups)





sf::st_write(acs_sf_blockgroups, df_complete_path, append = F)

block_groups <- block_groups %>% 
  select(bg_geoid, geometry)

sf::st_write(block_groups, blockgroups_geom_path, append = F)


##### REVISIT LATER - drop incidents that cannot be located in a bg #####
# Mapping for testing
acs_sf_blockgroups <- acs_sf_blockgroups %>%
  select(block_addr_100, zip, bg_geoid) %>%
  filter(!is.na(bg_geoid))

incident_counts <- acs_sf_blockgroups %>%
  st_set_geometry(NULL) %>%
  group_by(bg_geoid) %>%
  summarise(incident_count = n())

block_groups <- merge(block_groups, incident_counts, by = "bg_geoid", all.x = TRUE)
block_groups$incident_count <- ifelse(is.na(block_groups$incident_count), 0, block_groups$incident_count)

end_time <- Sys.time()
calc_duration(start_time, end_time)

breaks <- c(0, 1, 10, 25, 50, 100, 200, 500, 1000, 3000, max(block_groups$incident_count))
color_palette <- viridis::viridis(length(breaks))


block_groups$binned_incidents <- cut(block_groups$incident_count,
                                     breaks = breaks,
                                     include.lowest = TRUE,
                                     labels = FALSE)

# Convert binned incidents to factor for coloring purposes
block_groups$binned_incidents <- factor(block_groups$binned_incidents)

# Use the generated color palette and custom breaks in the mapview call
map <- mapview(block_groups, zcol = "incident_count",
               col.regions = color_palette,
               at = breaks,
               alpha.regions = 0.5)
map


