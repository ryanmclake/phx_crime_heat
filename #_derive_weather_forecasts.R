
remotes::install_github("FLARE-forecast/RopenMeteo")

library(RopenMeteo)
library(ggplot2)
library(dplyr)


sites <- read.csv("data/census_geolocated.csv") %>%
  filter(!is.na(long))

north <- RopenMeteo::get_ensemble_forecast(
  latitude = 34.06,
  longitude = -112.1044751777789,
  forecast_days = 7,
  past_days = 1,
  model = "gfs_seamless",
  variables = c("temperature_2m"))|>
  mutate(location = "north")

north_past <- RopenMeteo::get_forecast(
  latitude = 34.06,
  longitude = -112.1044751777789,
  forecast_days = 1,
  past_days = 92,
  model = "generic",
  variables = c("temperature_2m"))|>
  mutate(location = "north_past",
         ensemble = "0")

bind_rows(north_past, north) |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

south <- RopenMeteo::get_ensemble_forecast(
  latitude = 33.2,
  longitude = -112.08982679615825,
  forecast_days = 7,
  past_days = 1,
  model = "gfs_seamless",
  variables = c("temperature_2m")) |>
  mutate(location = "south")


phx_forecast <- bind_rows(north, south)



north_past |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)

south |> 
  mutate(variable = paste(variable, unit)) |> 
  ggplot(aes(x = datetime, y = prediction, color = ensemble)) + 
  geom_line() + 
  geom_vline(aes(xintercept = reference_datetime)) + 
  facet_wrap(~variable, scale = "free", ncol = 2)
