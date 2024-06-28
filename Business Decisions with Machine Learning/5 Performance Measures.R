library(h2o)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

# Initialize H2O
h2o.init()

# Load the Product Backorders dataset
data <- h2o.importFile("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/view-source_https___raw.githubusercontent.com_TUHHStartupEngineers_dat_sci_ss20_master_10_product_backo.csv")

#-------------------------------------
# Split the dataset into training and test sets
splits <- h2o.splitFrame(data, ratios = 0.8, seed = 1234)
train <- splits[[1]]
test <- splits[[2]]

#-------------------------------------
# Run AutoML
automl_models_h2o <- h2o.automl(
  x = setdiff(names(data), "went_on_backorder"),
  y = "went_on_backorder",
  training_frame = train,
  leaderboard_frame = test,
  max_models = 10,
  seed = 1234
)

# Visualize the leaderboard
automl_leaderboard <- automl_models_h2o@leaderboard
print(automl_leaderboard)

# Convert to tibble and visualize using ggplot2
leaderboard_tbl <- as_tibble(automl_leaderboard)

leaderboard_tbl %>%
  select(model_id, auc, logloss) %>%
  pivot_longer(cols = c("auc", "logloss"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = reorder(model_id, -value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Leaderboard Visualization", x = "Model ID", y = "Metric Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------
# Define hyperparameters
hyper_params <- list(
  hidden = list(c(50, 50), c(100, 100)),
  epochs = c(10, 20)
)

# Perform grid search
grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "dl_grid",
  hyper_params = hyper_params,
  training_frame = train,
  validation_frame = test,
  x = setdiff(names(data), "went_on_backorder"),
  y = "went_on_backorder",
  nfolds = 5
)

# Get the grid results
grid_perf <- h2o.getGrid(grid_id = "dl_grid", sort_by = "auc", decreasing = TRUE)
print(grid_perf)

#-------------------------------------
# Visualize the Trade-off between Precision and Recall and the Optimal Threshold
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)
perf <- h2o.performance(best_model, newdata = test)

# Extract precision-recall data
perf_tbl <- as_tibble(h2o.metric(perf))

# Plot precision vs recall
p1 <- ggplot(perf_tbl, aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue") +
  geom_line(aes(y = recall), color = "red") +
  labs(title = "Precision vs Recall", x = "Threshold", y = "Value") +
  theme_minimal()

#-------------------------------------
# Manual ROC Plot
roc_tbl <- as_tibble(h2o.performance(best_model, newdata = test)@metrics$thresholds_and_metric_scores) %>%
  select(fpr, tpr)

p2 <- ggplot(roc_tbl, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

#-------------------------------------
# Plot precision vs recall curve
p3 <- ggplot(perf_tbl, aes(x = recall, y = precision)) +
  geom_line(color = "purple") +
  labs(title = "Precision vs Recall", x = "Recall", y = "Precision") +
  theme_minimal()

#-------------------------------------
# Gain Plot
gain_lift_tbl <- as_tibble(h2o.gainsLift(perf))

# Plot Gain Chart
p4 <- ggplot(gain_lift_tbl, aes(x = cumulative_data_fraction, y = cumulative_capture_rate)) +
  geom_line(color = "green") +
  geom_abline(linetype = "dashed") +
  labs(title = "Gain Chart", x = "Cumulative Data Fraction", y = "Gain") +
  theme_minimal()

#-------------------------------------
# Lift Plot
p5 <- ggplot(gain_lift_tbl, aes(x = cumulative_data_fraction, y = cumulative_lift)) +
  geom_line(color = "orange") +
  geom_abline(linetype = "dashed") +
  labs(title = "Lift Chart", x = "Cumulative Data Fraction", y = "Lift") +
  theme_minimal()

#-------------------------------------
# Dashboard with Cowplot
plot_grid(p1, p2, p3, p4, p5, ncol = 2)

