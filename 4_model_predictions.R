if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(recipes)
library(ranger)
library(xgboost)
library(caret)
library(themis)
library(parsnip)
library(kernlab)
library(rsample)
library(yardstick)
library(keras)
library(workflows)



###

# app_df <- app_df %>%
#   mutate(zip = as.factor(zip),
#          premise_category = as.factor(premise_category),
#          lat = as.double(lat),
#          long = as.double(long)) %>% 
#   select(ucr_crime_category, occurred_on, zip, premise_category, lat, long)
# 
# # Data preprocessing
# data_prep <- recipe(ucr_crime_category ~ occurred_on + zip + premise_category + lat + long, data = app_df) %>%
#   step_date(occurred_on, features = c("month", "dow")) %>%
#   step_time(occurred_on, features = c("hour", "minute")) %>%
#   step_upsample(ucr_crime_category, over_ratio = .5, skip = FALSE) %>%
#   step_novel(all_nominal()) %>%
#   step_dummy(all_nominal(), -all_outcomes()) %>%
#   step_zv(all_predictors()) %>%  # Removing columns with zero variance
#   step_center(all_numeric(), -starts_with("occurred_on_"), -all_outcomes()) %>%
#   step_scale(all_numeric(), -starts_with("occurred_on_"), -all_outcomes()) %>%
#   prep(training = app_df, retain = TRUE)
# 
# prepared_data <- bake(data_prep, app_df)
# prepared_data <- prepared_data[order(prepared_data$occurred_on), ] #respect chronological order
# 
# # Splitting the dataset
# set.seed(3212024) # for reproducibility
# training_indices <- createDataPartition(prepared_data$ucr_crime_category, p = 0.8, list = FALSE)
# train_data <- prepared_data[training_indices, ]
# test_data <- prepared_data[-training_indices, ]
# actual_labels <- factor(test_data$ucr_crime_category, levels = levels(train_data$ucr_crime_category))
# 
# # Random Forest Model ####
# rf_model <- ranger(
#   ucr_crime_category ~ ., 
#   data = train_data, 
#   probability = TRUE, 
#   num.trees = 500
# )
# 
# # GBM Model with xgboost ####
# # Prepare the data for xgboost
# train_data_matrix <- model.matrix(~ . -1 - ucr_crime_category, data = train_data)
# train_labels <- as.numeric(as.factor(train_data$ucr_crime_category)) - 1 # Adjusting to 0-index if necessary
# train_matrix <- xgb.DMatrix(data = as.matrix(train_data_matrix), label = train_labels)
# 
# test_data_matrix <- model.matrix(~ . -1 - ucr_crime_category, data = test_data)
# test_labels <- as.numeric(as.factor(test_data$ucr_crime_category)) - 1
# test_matrix <- xgb.DMatrix(data = as.matrix(test_data_matrix), label = test_labels)
# 
# # Parameters for xgboost
# params <- list(
#   objective = "multi:softprob",
#   num_class = length(unique(app_df$ucr_crime_category)),
#   eval_metric = "mlogloss"
# )
# 
# watchlist <- list(train = train_matrix, eval = test_matrix)
# gbm_model <- xgb.train(
#   params = params,
#   data = train_matrix,
#   nrounds = 100,
#   watchlist = watchlist,
#   verbose = 0
# )
# gbm_predictions <- predict(gbm_model, test_matrix, pred)
# 
# 
# # SVM model ####
# svm_model <- svm_rbf(cost = 10, rbf_sigma = 0.1, prob) %>%
#   set_engine("kernlab") %>%
#   set_mode("classification")
# 
# # Step 2: Fit the model
# svm_fit <- svm_model %>%
#   fit(ucr_crime_category ~ ., data = train_data)
# 
# # After fitting the model, you can use it to make predictions
# # Example: Predicting on the test data
# test_data_predictions <- predict(svm_fit, test_data)
# #test_data_prob <- predict(svm_fit, test_data, type = "prob")
# 
# # To evaluate the model, you might want to calculate accuracy or other metrics
# test_data_results <- bind_cols(test_data, test_data_predictions)
# svm_accuracy <- test_data_results %>%
#   metrics(truth = ucr_crime_category, estimate = .pred_class) %>%
#   filter(.metric == "accuracy")
# 
# # Print the accuracy
# print(svm_accuracy)
# 
# 
# # GBM accuracy ####
# levels_train_labels <- levels(factor(train_data$ucr_crime_category))
# prob_matrix <- matrix(gbm_predictions, nrow = nrow(test_data), byrow = FALSE)
# 
# predicted_class_indices <- max.col(prob_matrix) - 1
# gbm_predicted_classes <- factor(predicted_class_indices, levels = 0:(length(levels_train_labels) - 1), labels = levels_train_labels)
# gbm_accuracy <- caret::confusionMatrix(gbm_predicted_classes, factor(test_data$ucr_crime_category, levels = levels_train_labels))
# 
# conf_matrix <- gbm_accuracy$table
# conf_matrix_long <- as.data.frame(as.table(conf_matrix))
# # Plotting
# ggplot(data = conf_matrix_long, aes(x = Reference, y = Prediction, fill = Freq)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(x = 'Predicted Label', y = 'Actual Label', fill = 'Count', title = 'Confusion Matrix Heatmap')
# 
# print(gbm_accuracy)
# 
# 
# 
# # RF accuracy ####
# rf_predictions <- predict(rf_model, test_data)$predictions
# 
# max_prob_indices <- max.col(rf_predictions)
# predicted_classes_rf <- levels(actual_labels)[max_prob_indices]
# predicted_labels_rf <- factor(predicted_classes_rf, levels = levels(actual_labels))
# rf_accuracy <- caret::confusionMatrix(predicted_labels_rf, actual_labels)
# 
# print(rf_accuracy)
# 
# conf_matrix <- rf_accuracy$table
# conf_matrix_long <- as.data.frame(as.table(conf_matrix))
# # Plotting
# ggplot(data = conf_matrix_long, aes(x = Reference, y = Prediction, fill = Freq)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(x = 'Predicted Label', y = 'Actual Label', fill = 'Count', title = 'Confusion Matrix Heatmap')



#### SHARED DATA PREP ####
# *************************
# *************************
raw_data_path <- here::here("data/crime_data_raw.csv")
census_geolocated_path <- here::here("data/census_geolocated.csv") # census API data
df_complete_path <- here::here("data/df_complete_forapp.gpkg")
blockgroups_geom_path <- here::here("data/blockgroups_geom.gpkg")

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
  # filter for now until performance is optimized
  slice_sample(n=50000, replace = F)

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

monthly_crimes <- app_df %>%
  mutate(month = floor_date(occurred_on, unit="month"), year = year(occurred_on)) %>%
  group_by(zip, year, month) %>%
  summarise(total_crimes = n(), .groups = 'drop')

# Split data
set.seed(123)
data_split <- initial_split(monthly_crimes, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Recipe
recipe <- recipe(total_crimes ~ zip + year + month, data = train_data) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(training = train_data, retain = TRUE)

# Model specification
model_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

regression_train_prepped <- bake(recipe, new_data = train_data) 
regression_test_prepped <- bake(recipe, new_data = test_data)

# Fit model
fit <- fit(model_spec, total_crimes ~ ., data = regression_train_prepped)

# Predictions
predictions <- predict(fit, new_data = regression_test_prepped) %>%
  bind_cols(test_data) %>%
  metrics(truth = total_crimes, estimate = .pred)

new_data <- tibble(
  zip = as.factor(c("85051", "85021", "85022", "85031", "85013")), # Ensure this matches the factor levels used in training
  year = 2024,
  month = as.Date("2024-04-01") # The date format should match your training data
)

new_data_with_predictions <- new_data %>%
  bake(recipe, .) %>%
  predict(fit, new_data = .) %>%
  bind_cols(new_data, prediction = .$`.pred`)

# Filter monthly_crimes for the specified zip codes and combine with predictions
combined_data <- monthly_crimes %>%
  filter(zip %in% c("85051", "85021", "85022", "85031", "85013"),
         year %in% c("2023")) %>% 
  bind_rows(select(new_data_with_predictions, zip, year, month, prediction))

combined_data$month <- as.Date(combined_data$month)

# Adding a type column to distinguish between actual counts and predictions
combined_data <- combined_data %>%
  mutate(type = ifelse(is.na(prediction), "Actual", "Prediction"))

#print(head(combined_data))

# Plotting with ggplot2
ggplot(combined_data, aes(x = month, y = ifelse(is.na(prediction), total_crimes, prediction), group = zip, color = zip)) +
  geom_line(size = 1) +  # Consistent line type across zip codes
  geom_point(aes(shape = type), size = 3) +  # Point shapes change according to type
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#34ccff")) +  # Custom colors for each zip code
  scale_shape_manual(values = c(16, 17)) +  # Custom shapes for Actual and Prediction
  labs(title = "Crime Counts and Predictions by Zip Code",
       subtitle = "Actual counts for 2023 and predictions for April 2024",
       x = "Month", y = "Counts / Predictions",
       color = "Zip Code", shape = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# app_df <- app_df %>%
#   mutate(
#     occurred_on = as.Date(occurred_on),
#     month = floor_date(occurred_on, unit = "month"),
#     year = year(occurred_on)
#   )
# 
# app_df <- left_join(app_df, monthly_crimes, by = c("zip", "month", "year"))
# 
# avg_lat_long_by_zip <- app_df %>%
#   group_by(zip) %>%
#   summarise(avg_lat = mean(lat, na.rm = TRUE),
#             avg_long = mean(long, na.rm = TRUE))
# 
# premise_freq_by_zip <- app_df %>%
#   count(zip, premise_category) %>%
#   group_by(zip) %>%
#   mutate(freq = n / sum(n)) %>%
#   select(-n) %>%
#   spread(key = premise_category, value = freq, fill = 0) %>% 
#   rename(
#     Commercial_Retail = `Commercial & Retail`,
#     Outdoor_Recreational = `Outdoor & Recreational`,
#     Public_Institutional = `Public & Institutional`,
#     Transport_Utilities = `Transport & Utilities`,
#     Other_Commercial = `Other Commercial`
#   )
# 
# zip_level_data <- avg_lat_long_by_zip %>%
#   inner_join(premise_freq_by_zip, by = "zip")
# 
# app_df_aggregated <- app_df %>%
#   select(zip, ucr_crime_category, occurred_on) %>%
#   distinct() %>%
#   left_join(zip_level_data, by = "zip")

#### REGRESSION MODEL ####
# *************************
# *************************





# Predict on the test set
rf_regression_predictions <- predict(rf_regression_model, test_data_regression)$predictions

test_data_regression$predicted_crimes = rf_regression_predictions
rmse <- sqrt(mean((test_data_regression$predicted_crimes - test_data_regression$total_crimes)^2))
print(paste("RMSE:", rmse))
print(paste("monthly overall avg:", mean(monthly_crimes$total_crimes)))



#### CATEGORICAL MODEL ####
# *************************
# *************************


data_prep_aggregated <- recipe(ucr_crime_category ~ ., data = app_df_aggregated) %>%
  step_date(occurred_on, features = c("year", "month", "dow")) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_rm(occurred_on) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep(training = app_df_aggregated, retain = TRUE)

prepared_data_aggregated <- bake(data_prep_aggregated, app_df_aggregated)
# prepared_data_aggregated <- prepared_data_aggregated[order(prepared_data_aggregated$occurred_on), ] #respect chronological order
# prepared_data_aggregated <- prepared_data_aggregated %>% 
#   select(-occurred_on)

set.seed(3212024) # for reproducibility
training_indices <- createDataPartition(prepared_data_aggregated$ucr_crime_category, p = 0.8, list = FALSE)
train_data_aggregated <- prepared_data_aggregated[training_indices, ]
test_data_aggregated <- prepared_data_aggregated[-training_indices, ]

rf_model_aggregated <- ranger(
  ucr_crime_category ~ ., 
  data = train_data_aggregated, 
  probability = TRUE, 
  num.trees = 500
)

#### CATEGORICAL & REGRESSION PREDICTIONS ####
# ********************************************
# ********************************************

zip_code_for_prediction = 85015
today_date <- Sys.Date()
prediction_occurred_on <- as.Date(format(today_date %m+% months(1), "%Y-%m-01"))

prediction_data_aggregated <- zip_level_data %>%
  filter(zip == as.character(zip_code_for_prediction)) %>%
  mutate(occurred_on = prediction_occurred_on) # Add these columns

prepared_prediction_data <- bake(data_prep_aggregated, new_data = prediction_data_aggregated)
predictions <- predict(rf_model_aggregated, prepared_prediction_data)$predictions

###
prediction_data_aggregated <- monthly_crimes %>%
  filter(zip == as.character(zip_code_for_prediction)) %>%
  mutate(occurred_on = prediction_occurred_on) # Add these columns

prepared_prediction_data <- bake(data_prep_regression, new_data = prediction_data_aggregated)

rf_regression_predictions <- predict(rf_regression_model, prepared_prediction_data)$predictions
rf_regression_predictions

