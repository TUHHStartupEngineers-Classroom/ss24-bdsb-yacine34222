# Load necessary libraries
library(tidyverse)
library(readr)
library(h2o)



#Set the working directory to the specified path
setwd("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning")

# Initialize H2O
h2o.init()

# Check if H2O cluster is running
if (!h2o.clusterIsUp()) {
  stop("H2O cluster is not running. Please check the H2O initialization.")
}

# Load the dataset
train_path <- "C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/view-source_https___raw.githubusercontent.com_TUHHStartupEngineers_dat_sci_ss20_master_10_product_backo.csv"
data <- read_csv(train_path)

# Inspect the data
glimpse(data)

# Specify the response and predictor variables
response <- "went_on_backorder"
predictors <- setdiff(names(data), response)

# Convert response variable to factor for classification
data[[response]] <- as.factor(data[[response]])

# Split the data into training and testing sets
split <- h2o.splitFrame(as.h2o(data), ratios = 0.85, seed = 1234)
train_h2o <- split[[1]]
test_h2o <- split[[2]]

# Run AutoML
automl_models <- h2o.automl(
  x = predictors,
  y = response,
  training_frame = train_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 600,
  nfolds = 5,
  seed = 1234
)

# View the leaderboard
leaderboard <- automl_models@leaderboard
print(leaderboard)

# Get the best model
best_model <- automl_models@leader

# Make predictions using the best model
predictions <- h2o.predict(best_model, test_h2o)
predictions_df <- as.data.frame(predictions)

# Save the leader model
model_path <- h2o.saveModel(object = best_model, path = getwd(), force = TRUE)
print(paste("Model saved to:", model_path))

# Shutdown H2O
h2o.shutdown(prompt = FALSE)

