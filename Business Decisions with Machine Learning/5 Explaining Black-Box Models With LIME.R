
 
# Load Libraries
library(h2o)
library(readxl)
library(tidyverse)
library(recipes)
library(lime)

# Initialize H2O
h2o.init()

# Load Data
employee_attrition_tbl <- read_csv("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.txt")

# Data Preprocessing
# Convert necessary columns to factors
employee_attrition_tbl <- employee_attrition_tbl %>%
  mutate(
    Attrition = as.factor(Attrition),
    BusinessTravel = as.factor(BusinessTravel),
    Department = as.factor(Department),
    EducationField = as.factor(EducationField),
    Gender = as.factor(Gender),
    JobRole = as.factor(JobRole),
    MaritalStatus = as.factor(MaritalStatus),
    OverTime = as.factor(OverTime)
  )

# Split into training and test sets
set.seed(1113)
split_obj <- initial_split(employee_attrition_tbl, prop = 0.85)
train_tbl <- training(split_obj)
test_tbl <- testing(split_obj)

# ML Preprocessing Recipe
recipe_obj <- recipe(Attrition ~ ., data = train_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c("JobLevel", "StockOptionLevel"), fn = as.factor) %>%
  prep()

train_processed_tbl <- bake(recipe_obj, new_data = train_tbl)
test_processed_tbl <- bake(recipe_obj, new_data = test_tbl)



# Train an H2O model
h2o.init()

# Convert to H2O frames
train_h2o <- as.h2o(train_processed_tbl)
test_h2o <- as.h2o(test_processed_tbl)

# Train H2O model using AutoML
automl_leader <- h2o.automl(y = "Attrition", training_frame = train_h2o, max_runtime_secs = 600)

# Get the best model
best_model <- automl_leader@leader

# Generate Predictions
predictions_tbl <- h2o.predict(best_model, newdata = test_h2o) %>%
  as_tibble() %>%
  bind_cols(test_processed_tbl %>% select(Attrition, EmployeeNumber))

# Create Explainer
explainer <- lime::lime(
  train_processed_tbl %>% select(-Attrition),
  model = best_model,
  bin_continuous = TRUE,
  n_bins = 4,
  quantile_bins = TRUE
)

# Generate Explanation for Single Instance
explanation <- lime::explain(
  x = test_processed_tbl %>% slice(1) %>% select(-Attrition),
  explainer = explainer,
  n_labels = 1,
  n_features = 8,
  n_permutations = 5000,
  kernel_width = 1
)
library(ggplot2)

# Convert explanation to tibble
explanation_tbl <- as_tibble(explanation)

# Filter for the first case
case_1 <- explanation_tbl %>%
  filter(case == 1)

# Create the custom plot_features
ggplot(case_1, aes(x = reorder(feature, feature_weight), y = feature_weight)) +
  geom_col(fill = 'steelblue') +
  coord_flip() +
  labs(title = 'Feature Importance for Case 1', x = 'Feature', y = 'Feature Weight') +
  theme_minimal()


#---------------------
#Part 2: Plot Explanations for Multiple Instances

# Generate Explanations for Multiple Instances
explanation_multiple <- lime::explain(
  x = test_processed_tbl %>% slice(1:20) %>% select(-Attrition),
  explainer = explainer,
  n_labels = 1,
  n_features = 8,
  n_permutations = 5000,
  kernel_width = 0.5
)

# Convert to tibble
explanation_multiple_tbl <- as_tibble(explanation_multiple)

# Plot Explanations
ggplot(explanation_multiple_tbl, aes(x = feature, y = case, fill = feature_weight)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(min(explanation_multiple_tbl$feature_weight),
                                               max(explanation_multiple_tbl$feature_weight)), space = "Lab",
                       name="Feature Weight") +
  labs(title = 'Feature Importance Across Multiple Cases', x = 'Feature', y = 'Case') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

