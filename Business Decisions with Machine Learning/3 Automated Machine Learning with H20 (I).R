# Load necessary libraries
library(tidyverse)
library(readr)
library(GGally)

# Load the dataset with comma delimiter
employee_attrition_tbl <- read_delim("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.txt", delim = ",")

# Display the column names to verify the correct names
print(colnames(employee_attrition_tbl))

# Display the first few rows of the dataset to understand the structure
print(head(employee_attrition_tbl))

# Define the correct column name for attrition
correct_attrition_column <- "Attrition"

# Define the custom plotting function
plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  color_expr <- enquo(color)
  if (rlang::quo_is_null(color_expr)) {
    g <- data %>% ggpairs(lower = "blank") 
  } else {
    color_name <- quo_name(color_expr)
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  return(g)
}

# Analyzing Compensation Features

# 1. Monthly Income and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), MonthlyIncome), color = !!sym(correct_attrition_column)))

# 2. Percent Salary Hike and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), PercentSalaryHike), color = !!sym(correct_attrition_column)))

# 3. Stock Option Level and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), StockOptionLevel), color = !!sym(correct_attrition_column)))

# Analyzing Survey Results

# 4. Environment Satisfaction and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), EnvironmentSatisfaction), color = !!sym(correct_attrition_column)))

# 5. Work Life Balance and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), WorkLifeBalance), color = !!sym(correct_attrition_column)))

# Analyzing Performance Data

# 6. Job Involvement and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), JobInvolvement), color = !!sym(correct_attrition_column)))

# Analyzing Work-Life Features

# 7. Over Time and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), OverTime), color = !!sym(correct_attrition_column)))

# Analyzing Training and Education

# 8. Training Times Last Year and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), TrainingTimesLastYear), color = !!sym(correct_attrition_column)))

# Analyzing Time-Based Features

# 9. Years At Company and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), YearsAtCompany), color = !!sym(correct_attrition_column)))

# 10. Years Since Last Promotion and Attrition
print(plot_ggpairs(employee_attrition_tbl %>% select(all_of(correct_attrition_column), YearsSinceLastPromotion), color = !!sym(correct_attrition_column)))
