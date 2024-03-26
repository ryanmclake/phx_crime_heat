library(tidyverse)
library(here)
library(yardstick)
library(fable)
library(fabletools)
library(feasts) # for additional diagnostics
library(tsibble)
library(fable)
library(fabletools)
library(dplyr)
library(lubridate)
library(ggplot2)
#install.packages("fable.prophet")

#### SHARED DATA PREP ####
# *************************
# *************************
raw_data_path <- here::here("data/crime_data_raw.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv") # census API data
#df_complete_path <- here::here("data/df_complete_forapp.gpkg")
#blockgroups_geom_path <- here::here("data/blockgroups_geom.gpkg")

raw_df <- readr::read_csv(raw_data_path) %>%
  rename(block_addr_100 = `100_block_addr`) %>% 
  mutate(zip = as.character(zip)) 

geo_df <- read_csv(census_geolocated_path) %>%
  filter(!is.na(lat),
         !is.na(long),
         `100_block_addr` != "100_block_addr") %>% 
  mutate(zip = as.character(zip),
         lat = as.numeric(lat),
         long = as.numeric(long)
  ) %>%
  rename(block_addr_100 = `100_block_addr`) %>% 
  distinct(block_addr_100, zip, .keep_all = TRUE) %>% 
  select(c("block_addr_100", "zip", "lat", "long"))

app_df <- raw_df %>%
  left_join(geo_df, by = c("block_addr_100", "zip")) %>%
  select(-state, -occurred_to, -block_addr_100, -grid, -state) %>% 
  filter(!is.na(lat),!is.na(long)) %>% 
  slice_sample(n=20000, replace = F)

app_df <- app_df %>%
  mutate(premise_category = case_when(
    str_detect(premise_type, "OFFICE|COMMERCIAL|RENTAL STORAGE|FACILITY|WAREHOUSE|FACTORY|PARKING GARAGE") | str_detect(premise_type, "STOREROOM/SHED \\(COMMERCIAL\\)") ~ "Other Commercial",
    str_detect(premise_type, "SINGLE FAMILY HOUSING|HOUSE|APARTMENT|MOBILE|CONDO|TOWNHOUSE|GARAGE|CARPORT|DRIVEWAY|FENCED RESIDENTIAL YARD|GROUP HOME|HOTEL|MOTEL") | str_detect(premise_type, "STOREROOM/SHED \\(RESIDENTIAL\\)") ~ "Residential",
    str_detect(premise_type, "GOVERNMENT|PUBLIC|SCHOOL|HOSPITAL|LIGHT RAIL FACILITY|MEDICAL|CHILD CARE|DAY CARE|CHURCH|SYNAGOGUE|TEMPLE|MOSQUE|AIRPORT|COMMUNITY CENTER|NURSING CARE") ~ "Public & Institutional",
    str_detect(premise_type, "MARKET|STORE|DEPARTMENT|DISCOUNT|RETAIL|FAST FOOD|GAS|GROCERY|SUPER MARKET|BANK|SAVINGS|CREDIT UNION|RESTAURANT|DRUG|SHOPPING MALL|THEATRE|BAR|LOUNGE|NIGHT CLUB|ADULT ONLY|MOVIE") ~ "Commercial & Retail",
    str_detect(premise_type, "PARK|PLAYGROUND|PARKING LOT|STREET|ROADWAY|SIDEWALK|ALLEY|CONSTRUCTION|OPEN SPACE|DESERT|FIELD|WOODS|FENCED YARD|PARKING GARAGE") ~ "Outdoor & Recreational",
    str_detect(premise_type, "VEHICLE|BUS STOP|LIGHT RAIL|TRAIN|BUS|FACILITY") ~ "Transport & Utilities",
    str_detect(premise_type, "UNKNOWN|OTHER|ABANDONED|CONDEMNED") ~ "Miscellaneous",
    TRUE ~ "Miscellaneous" # Catch-all for anything not matched
  ))

app_df <- app_df %>%
  mutate(premise_category = as.factor(premise_category),
         zip = as.factor(zip),
         ucr_crime_category = as.factor(ucr_crime_category))

# app_df %>%
#   group_by(ucr_crime_category) %>%
#   summarise(Count = n()) %>%
#   mutate(Frequency_Percentage = (Count / sum(Count)) * 100) %>%
#   select(ucr_crime_category, Frequency_Percentage) %>% 
#   arrange(desc(Frequency_Percentage))



#### MODELING PRACTICE WITH ARIMA ##########
# *************************
# *************************

# Step 1: Aggregate data to weekly totals across all zip codes
df_weekly <- app_df %>%
  mutate(week_start = as.Date(floor_date(occurred_on, unit = "week"))) %>%
  group_by(week_start) %>%
  summarise(total_crimes = n(), .groups = "drop") %>% # Correctly count incidents per week
  filter(week_start <= "2024-02-11") %>% #filter out an incomplete week
  as_tsibble(index = week_start)

df_weekly %>%
  autoplot(total_crimes)

# Step 2: Split into training and test sets (e.g., last 4 observations as test set)
split_point <- round(nrow(df_weekly) * 0.8)
train_set <- df_weekly %>% slice(1:split_point)
test_set <- df_weekly %>% slice((split_point + 1):n())
library(fable.prophet)

# Step 3: Fit ARIMA and ETS models
models <- train_set %>%
  model(
    #arima_model = ARIMA(total_crimes),
    #ets_model = ETS(total_crimes),
    prophet_model = fable.prophet::prophet(total_crimes ~ season(period = 52,
                                                           order = 2,
                                                           type = "multiplicative")
  ))

fitted_models <- models %>% 
  forecast(h = nrow(test_set))

accuracy_forecasts <- fabletools::accuracy(fitted_models, test_set)
accuracy_forecasts

models %>% 
  gg_tsresiduals()




# autoplot(forecasts) +
#   autolayer(test_set, total_crimes) +
#   labs(title = "Forecasts vs Actual Data",
#        x = "Time", y = "Total Crimes",
#        caption = "Comparison between forecasted values and actual observations.") +
#   theme_minimal()

totalcrimes_forecast <- df_weekly %>% 
  model(ETS(total_crimes)) %>% 
  forecast(h = 10)

# totalcrimes_forecast %>% 
#   autoplot(df_weekly %>% 
#              filter(week_start > "2024-01-16"))

totalcrimes_forecast %>% 
  autoplot(df_weekly %>% 
             filter(week_start > "2022-01-16"))


totalcrimes_forecast <- as_tibble(totalcrimes_forecast) %>% 
  mutate(total_crimes = .mean,
         total_crimes_rounded = round(total_crimes)) %>% 
  select(week_start, total_crimes)


## Weekly counts
weekly_crime_counts <- app_df %>%
  mutate(week_start = floor_date(occurred_on, unit = "week")) %>%
  filter(week_start <= "2024-02-11") %>% #filter out an incomplete week
  count(week_start, ucr_crime_category) %>%
  complete(week_start, ucr_crime_category, fill = list(n = 0))

weekly_total_crimes <- weekly_crime_counts %>%
  group_by(week_start) %>%
  summarise(total_crimes = sum(n))

weekly_crime_fractions <- weekly_crime_counts %>%
  left_join(weekly_total_crimes, by = "week_start") %>%
  mutate(fraction = n / total_crimes) %>%
  select(-total_crimes, -n) %>% 
  mutate(week_start = as.Date(week_start)) %>% 
  as_tsibble(index = week_start, key = ucr_crime_category) %>% 
  fill_gaps() %>%
  mutate(fraction = replace_na(fraction, 0))

fit <- weekly_crime_fractions %>%
  model(
    HL = ETS(fraction ~ error("A") + trend("A") + season("N"))
  )

crimetypes_forecast <- forecast(fit, h = 4)

forecasts_df <- as_tibble(crimetypes_forecast)

forecasts_df <- forecasts_df %>%
  group_by(week_start) %>%  
  mutate(normalized_fraction = .mean / sum(.mean)) %>%
  ungroup()

fit %>%
  forecast(h = 4) %>%
  autoplot(weekly_crime_fractions %>%
             filter(week_start > "2023-11-01"))


## estimate crimes
crime_estimates <- forecasts_df %>%
  left_join(totalcrimes_forecast, by = "week_start") %>%
  mutate(est_crimes = normalized_fraction * total_crimes,
         est_crimes_rounded = round(est_crimes))

ggplot(crime_estimates, aes(x = week_start, y = est_crimes, color = ucr_crime_category)) +
  geom_line() +  # Use geom_line() if you want to connect points with lines; replace with geom_point() for scatter plot
  geom_point() + # Adds the data points; remove if only lines are desired
  labs(title = "Estimated Crime Types Over Forecast Period",
       x = "Week Starting",
       y = "Estimated Number of Crimes Rounded",
       color = "Crime Type") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + # Adjust date format as needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Modeling by zip code and weekly data ####
zip_totals <- app_df %>%
  group_by(zip) %>%
  summarise(total_crimes = n(), .groups = "drop")

ggplot(zip_totals, aes(x = total_crimes)) +
  geom_histogram(binwidth = 500, fill = "grey", color = "black") + # Adjust binwidth as needed
  labs(title = "Distribution of Crimes Across Zip Codes",
       x = "Total Crimes",
       y = "Number of Zip Codes") +
  theme_minimal()

set.seed(123) # Set a seed for reproducibility
k_means_result <- kmeans(zip_totals$total_crimes, centers = 3)

# Add the cluster assignments to your dataframe
zip_totals <- zip_totals %>%
  mutate(cluster = k_means_result$cluster)

# Now you can filter your dataframe by the cluster assignment
zip_group_1 <- zip_totals %>% filter(cluster == 1)
zip_group_2 <- zip_totals %>% filter(cluster == 2)
zip_group_3 <- zip_totals %>% filter(cluster == 3)

# And if you want to visualize the groups on the histogram:
ggplot(zip_totals, aes(x = total_crimes, fill = factor(cluster))) +
  geom_histogram(binwidth = 500, alpha = 0.6) +
  labs(title = "Distribution of Crimes Across Zip Codes with Clusters",
       x = "Total Crimes",
       y = "Number of Zip Codes",
       fill = "Cluster") +
  theme_minimal()

app_df_group_1 <- app_df %>% 
  filter(zip %in% zip_group_1$zip)

# 2. Aggregate data weekly
df_weekly_group_1 <- app_df_group_1 %>%
  mutate(week_start = as.Date(floor_date(occurred_on, unit = "week"))) %>%
  group_by(zip, week_start) %>%
  summarise(total_crimes = n(), .groups = "drop") %>%
  as_tsibble(index = week_start, key = zip) %>%
  fill_gaps() %>%
  mutate(total_crimes = replace_na(total_crimes, 0))

split_point <- round(nrow(df_weekly_group_1) * 0.8)

# Split into training and test sets (based on 80/20 split)
train_set_group_1 <- df_weekly_group_1 %>% 
  slice(1:split_point)
test_set_group_1 <- df_weekly_group_1 %>% 
  slice((split_point + 1):n())

models_group_1 <- train_set_group_1 %>%
  model(prophet_model = fable.prophet::prophet(total_crimes))

# Forecast future crime counts
fitted_models_group_1 <- models_group_1 %>% 
  forecast(h = nrow(test_set_group_1))

# Evaluate the forecast against the test set
accuracy_forecasts_group_1 <- accuracy(fitted_models_group_1, test_set_group_1)
print(accuracy_forecasts_group_1)

# Optionally, visualize the forecast against actual data
autoplot(fitted_models_group_1) +
  autolayer(test_set_group_1, total_crimes) +
  labs(title = "Forecasts vs Actual Data for Zip Group 1",
       x = "Time", y = "Total Crimes",
       caption = "Comparison between forecasted values and actual observations.") +
  theme_minimal()


# FUNCTIONS ####
# *************************
# *************************
library(recipes)
library(ranger)
#library(xgboost)
library(caret)
library(themis)
library(parsnip)
library(kernlab)
library(rsample)
library(keras)
library(pheatmap)

gen_synthetic_crimes <- function(zip_code_list, predicted_total_crimes_per_zip, historical_data, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  date_sequence <- seq.Date(start_date, end_date, by = "day")
  
  synthetic_data <- lapply(seq_along(zip_code_list), function(i) {
    zip_code <- zip_code_list[i]
    total_crimes <- predicted_total_crimes_per_zip[i]
    
    zip_data <- historical_data %>%
      filter(zip == zip_code) %>%
      mutate(hour_minute = sprintf("%02d:%02d", hour(occurred_on), minute(occurred_on))) %>%
      group_by(hour_minute) %>%
      summarise(n = n(), .groups = 'drop') %>%
      mutate(prob = n / sum(n))
    
    sampled_hour_minutes <- sample(zip_data$hour_minute, total_crimes, replace = TRUE, prob = zip_data$prob)
    sampled_dates <- sample(date_sequence, total_crimes, replace = TRUE)
    
    synthetic_datetimes <- mapply(function(date, hour_minute) {
      as.POSIXct(paste(date, hour_minute), format = "%Y-%m-%d %H:%M", tz = "UTC")
    }, sampled_dates, sampled_hour_minutes, SIMPLIFY = FALSE)
    
    premise_distribution <- historical_data %>%
      filter(zip == zip_code) %>%
      count(premise_category) %>%
      mutate(prob = n / sum(n))
    
    synthetic_premises <- sample(premise_distribution$premise_category, total_crimes, replace = TRUE, prob = premise_distribution$prob)
    
    data.frame(
      zip = rep(zip_code, total_crimes),
      occurred_on = unlist(synthetic_datetimes),
      premise_category = factor(synthetic_premises, levels = c("Residential", "Other Commercial", "Outdoor & Recreational", "Miscellaneous", "Commercial & Retail", "Transport & Utilities", "Public & Institutional"))
    )
  }) %>%
    bind_rows()
  
  synthetic_data <- synthetic_data %>% 
    mutate(occurred_on = as.POSIXct(occurred_on, origin = "1970-01-01", tz = "UTC"),
           zip = as.factor(zip))
  
  return(synthetic_data)
}

# OVERALL VARIABLES ####
start_date <- Sys.Date()
end_date <- start_date %m+% months(1) - days(1)
date_sequence <- seq.Date(start_date, end_date, by = "day")
#zip_list <- c("85051", "85021", "85022", "85031", "85013")
zip_list <- c("85051", "85021")



#### REGRESSION MODELING ####
# *************************
# *************************
daily_crimes <- app_df %>%
  mutate(date = as.Date(occurred_on), year = year(occurred_on), month = month(occurred_on), day = day(occurred_on)) %>%
  group_by(zip, year, month, day) %>%
  summarise(total_crimes = n(), .groups = 'drop')

# Split data
set.seed(123)
data_split_daily <- initial_split(daily_crimes, prop = 0.8)
regr_train_data <- training(data_split_daily)
regr_test_data <- testing(data_split_daily)

# Recipe
regr_recipe <- recipe(total_crimes ~ zip + year + month + day, data = regr_train_data) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(training = regr_train_data, retain = TRUE)

# Model specification
regr_model_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

regr_train_prepped <- bake(regr_recipe, new_data = regr_train_data) 
regr_test_prepped <- bake(regr_recipe, new_data = regr_test_data)

# Fit model
regr_fit <- fit(regr_model_spec, total_crimes ~ ., data = regr_train_prepped)

predictions <- predict(regr_fit, new_data = regr_test_prepped) %>%
  bind_cols(regr_test_data) %>%
  metrics(truth = total_crimes, estimate = .pred)

predictions


# Specific Predictions
new_data_daily <- crossing(zip = factor(zip_list), date = date_sequence) %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) %>%
  select(-date) # Removing the date column if only year, month, and day are needed

daily_predictions <- new_data_daily %>%
  bake(regr_recipe, new_data = .) %>%
  predict(regr_fit, new_data = .) %>%
  mutate(prediction = .$.pred) %>%
  select(-.pred) %>%
  bind_cols(new_data_daily) %>% 
  select(zip, year, month, day, prediction)

pred_forecast <- daily_predictions %>%
  group_by(zip) %>%
  summarise(total_crimes = sum(prediction), .groups = 'drop')

regr_pred <- round(pred_forecast$total_crimes)

daily_crimes <- daily_crimes %>%
  mutate(type = "actual") %>% 
  filter(zip %in% zip_list)

daily_predictions <- daily_predictions %>%
  mutate(
    total_crimes = prediction, # Move the predictions to a column named 'total_crimes'
    type = "predicted"
  ) %>%
  select(-prediction) # Optionally remove the 'prediction' column, now redundant

combined_data <- bind_rows(daily_crimes, daily_predictions) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  filter(year>="2024")

# Plotting with ggplot2
ggplot(combined_data, aes(x = date, y = total_crimes, group = zip, color = zip)) +
  geom_line(size = 1) +  # Consistent line type across zip codes
  geom_point(aes(shape = type), size = 3) +  # Point shapes change according to type
  #scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#34ccff")) +  # Custom colors for each zip code
  scale_shape_manual(values = c(16, 17)) +  # Custom shapes for Actual and Prediction
  labs(title = "Crime Counts and Predictions by Zip Code",
       subtitle = "Actual counts for 2023 and predictions for April 2024",
       x = "Month", y = "Counts / Predictions",
       color = "Zip Code", shape = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### CLASSIFICATION MODELING ####
# *************************
# *************************
set.seed(123)
data_split <- initial_split(app_df, prop = 0.80)
class_train_data <- training(data_split)
class_test_data <- testing(data_split)

class_train_recipe <- recipe(ucr_crime_category ~ zip + premise_category + occurred_on, data = class_train_data) %>%
  step_upsample(ucr_crime_category, over_ratio = .5, skip = FALSE) %>%
  step_date(occurred_on, features = c("year", "month", "dow")) %>%
  step_holiday() %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(training = class_train_data, retain = TRUE)

# Recipe for testing (without step_upsample)
class_test_recipe <- recipe(ucr_crime_category ~ zip + premise_category + occurred_on, data = class_train_data) %>%
  step_date(occurred_on, features = c("year", "month", "dow")) %>%
  step_holiday() %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(training = class_train_data, retain = TRUE)

class_model_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")

class_train_prepped <- bake(class_train_recipe, new_data = class_train_data) 
class_test_prepped <- bake(class_test_recipe, new_data = class_test_data)

class_fit <- fit(class_model_spec, ucr_crime_category ~ ., data = class_train_prepped)

class_test_preds <- predict(class_fit, new_data = class_test_prepped, type = "prob")
class_test_preds <- bind_cols(class_test_preds, test_data %>% select(ucr_crime_category))

# Example of converting probabilities to class predictions
class_test_preds <- class_test_preds %>%
  mutate(predicted_class = max.col(select(., starts_with(".pred_")), ties.method = "first")) %>%
  mutate(predicted_class = names(select(., starts_with(".pred_")))[predicted_class]) %>%
  mutate(predicted_class = str_remove(predicted_class, "^\\.pred_"))

class_test_preds$ucr_crime_category <- factor(class_test_preds$ucr_crime_category)
class_test_preds$predicted_class <- factor(class_test_preds$predicted_class, levels = levels(class_test_preds$ucr_crime_category))

accuracy_metric <- accuracy_vec(truth = class_test_preds$ucr_crime_category, estimate = class_test_preds$predicted_class)
conf_matrix <- conf_mat(class_test_preds, truth = ucr_crime_category, estimate = predicted_class)
precision_metric <- precision(class_test_preds, truth = ucr_crime_category, estimate = predicted_class, event_level = "macro")
recall_metric <- recall(class_test_preds, truth = ucr_crime_category, estimate = predicted_class, event_level = "macro")
f1_metric <- f_meas(class_test_preds, truth = ucr_crime_category, estimate = predicted_class, event_level = "macro")
print(conf_matrix)
print(accuracy_metric)
print(precision_metric)
print(recall_metric)
print(f1_metric)

class_test_preds$predicted_class <- factor(class_test_preds$predicted_class)
actual_labels <- factor(class_test_preds$ucr_crime_category)
conf_matrix <- confusionMatrix(class_test_preds$predicted_class, actual_labels)
conf_matrix_table <- conf_matrix$table
pheatmap(conf_matrix_table, 
         color = colorRampPalette(c("white", "red"))(50), # Adjust colors as needed
         display_numbers = TRUE, # Show numbers on the heatmap
         cluster_rows = FALSE, 
         cluster_cols = FALSE,
         annotation_legend = FALSE)



# Synthetic data creation & prediction
synthetic_testdata <- gen_synthetic_crimes(zip_list, regr_pred, app_df, start_date, end_date)

synthetic_data_processed <- synthetic_testdata %>%
  bake(class_recipe, new_data = .)

classification_preds <- predict(class_fit, synthetic_data_processed) %>% 
  mutate(ucr_crime_category = .pred_class,
         crime_type = "predicted") %>% 
  select(-.pred_class)

class_data_complete <- bind_cols(synthetic_testdata, classification_preds) 




# First, aggregate data to get counts for each crime type, regardless of the zip code
crime_type_counts <- class_data_complete %>%
  group_by(ucr_crime_category, zip) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

ggplot(crime_type_counts, aes(x = reorder(ucr_crime_category, -count), y = count, fill = zip)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Crime Type Counts by Zip Code",
       x = "Crime Type",
       y = "Counts") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3")

# SAVING MODELS ####
models_path <- here::here("models/")
saveRDS(class_fit, file = here(models_path, "class_fit.rds"))
saveRDS(class_recipe, file = here(models_path, "class_recipe.rds"))
saveRDS(regr_fit, file = here(models_path, "regr_fit.rds"))
saveRDS(regr_recipe, file = here(models_path, "regr_recipe.rds"))

