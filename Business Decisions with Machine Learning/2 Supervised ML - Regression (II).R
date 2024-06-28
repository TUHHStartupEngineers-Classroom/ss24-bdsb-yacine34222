# Load necessary libraries
library(tidyverse)
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)
library(workflows)

# Load data
bike_features_tbl <- readRDS("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/bike_features_tbl.rds")

# Data preparation
bike_features_tbl <- bike_features_tbl %>% 
  select(model, `Rear Derailleur`, `Shift Lever`, price, category_2, frame_material) %>% 
  mutate(
    `shimano dura-ace`        = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano dura-ace"), 1, 0),
    `shimano ultegra`         = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano ultegra"), 1, 0),
    `shimano 105`             = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano 105"), 1, 0),
    `shimano tiagra`          = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano tiagra"), 1, 0),
    `shimano sora`            = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano sora"), 1, 0),
    `shimano deore`           = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano deore(?! xt)"), 1, 0),
    `shimano slx`             = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano slx"), 1, 0),
    `shimano grx`             = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano grx"), 1, 0),
    `shimano xt`              = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano deore xt|shimano xt"), 1, 0),
    `shimano xtr`             = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano xtr"), 1, 0),
    `shimano saint`           = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "shimano saint"), 1, 0),
    `sram red`                = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram red"), 1, 0),
    `sram force`              = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram force"), 1, 0),
    `sram rival`              = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram rival"), 1, 0),
    `sram apex`               = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram apex"), 1, 0),
    `sram xx1`                = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram xx1"), 1, 0),
    `sram x01`                = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram x01|sram xo1"), 1, 0),
    `sram gx`                 = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram gx"), 1, 0),
    `sram nx`                 = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram nx"), 1, 0),
    `sram sx`                 = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "sram sx"), 1, 0),
    `campagnolo potenza`      = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "campagnolo potenza"), 1, 0),
    `campagnolo super record` = ifelse(str_detect(str_to_lower(`Rear Derailleur`), "campagnolo super record"), 1, 0),
    `shimano nexus`           = ifelse(str_detect(str_to_lower(`Shift Lever`), "shimano nexus"), 1, 0),
    `shimano alfine`          = ifelse(str_detect(str_to_lower(`Shift Lever`), "shimano alfine"), 1, 0)
  ) %>% 
  select(-c(`Rear Derailleur`, `Shift Lever`)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

# Add ID column and arrange columns
bike_features_tbl <- bike_features_tbl %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

#------------------------------------------------------------------------------------
# Create a recipe for the data
recipe_obj <- recipe(price ~ ., data = bike_features_tbl) %>%
  step_rm(id, model) %>% # Removing non-predictive columns
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% # One-hot encoding for categorical variables
  step_novel(all_nominal(), -all_outcomes()) # Handle novel levels

# Splitting the data into training and testing sets
set.seed(1113)
split_obj <- initial_split(bike_features_tbl, prop = 0.80, strata = "category_2")

train_tbl <- training(split_obj)
test_tbl <- testing(split_obj)

#---------------------------------------------------------

# Create a linear regression model specification
linear_spec <- linear_reg(mode = "regression") %>% 
  set_engine("lm")

# Create a workflow
workflow_obj <- workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(linear_spec)

# Fit the model
fit_obj <- workflow_obj %>%
  fit(data = train_tbl)

#--------------------------------------------------------------------
# Make predictions on the test set
predictions <- predict(fit_obj, new_data = test_tbl)

# Bind predictions to test data
results <- test_tbl %>%
  bind_cols(predictions) %>%
  rename(.pred = .pred)

# Calculate performance metrics
metrics <- results %>%
  metrics(truth = price, estimate = .pred)

print(metrics)
