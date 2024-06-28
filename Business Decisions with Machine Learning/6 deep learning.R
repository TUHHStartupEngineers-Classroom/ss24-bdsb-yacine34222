# Load and inspect the data
library(tidyverse)
library(keras)
library(lime)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

churn_data_raw <- read_csv("C:/Users/Yacine Azouz/OneDrive/Documents/GitHub/ss24-bdsb-yacine34222/Business Decisions with Machine Learning/Business Decisions with Machine Learning/WA_Fn-UseC_-Telco-Customer-Churn.csv")

 glimpse(churn_data_raw)

 
 
 churn_data_tbl <- churn_data_raw %>%
   select(-customerID) %>%
   drop_na() %>%
   mutate(Churn = ifelse(Churn == "Yes", 1, 0))
 
 
 
 set.seed(100)
 train_test_split <- initial_split(churn_data_tbl, prop = 0.80)
 train_tbl <- training(train_test_split)
 test_tbl <- testing(train_test_split)
 
 
 
 churn_data_tbl %>% ggplot(aes(x = tenure)) + 
   geom_histogram(bins = 6, color = "white", fill =  "#2DC6D6") +
   labs(title = "Tenure Counts With Six Bins", x = "tenure (month)")
 
 train_tbl %>%
   select(Churn, TotalCharges) %>%
   mutate(
     Churn = Churn %>% as.factor() %>% as.numeric(),
     LogTotalCharges = log(TotalCharges)
   ) %>%
   correlate() %>%
   focus(Churn) %>%
   fashion()
 
 
 rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
   step_rm(Churn) %>%
   step_discretize(tenure, options = list(cuts = 6)) %>%
   step_log(TotalCharges) %>%
   step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
   step_center(all_predictors(), -all_outcomes()) %>%
   step_scale(all_predictors(), -all_outcomes()) %>%
   prep(data = train_tbl)
 
 x_train_tbl <- bake(rec_obj, new_data = train_tbl)
 x_test_tbl  <- bake(rec_obj, new_data = test_tbl)
 
 y_train_vec <- ifelse(train_tbl$Churn == "Yes", 1, 0)
 y_test_vec  <- ifelse(test_tbl$Churn == "Yes", 1, 0)
 
 
 
 model_keras <- keras_model_sequential()
 
 model_keras %>% 
   layer_dense(units = 16, kernel_initializer = "uniform", activation = "relu", input_shape = ncol(x_train_tbl)) %>% 
   layer_dropout(rate = 0.1) %>%
   layer_dense(units = 16, kernel_initializer = "uniform", activation = "relu") %>% 
   layer_dropout(rate = 0.1) %>%
   layer_dense(units = 1, kernel_initializer = "uniform", activation = "sigmoid") %>%
   compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = c('accuracy'))
 
 model_keras
 
 
 
 fit_keras <- model_keras %>% fit(
   x = as.matrix(x_train_tbl), 
   y = y_train_vec, 
   batch_size = 50, 
   epochs = 35, 
   validation_split = 0.3
 )
 
 
 yhat_keras_class_vec <- model_keras %>% predict_classes(as.matrix(x_test_tbl)) %>% as.vector()
 yhat_keras_prob_vec  <- model_keras %>% predict_proba(as.matrix(x_test_tbl)) %>% as.vector()
 
 estimates_keras_tbl <- tibble(
   truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
   estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
   class_prob = yhat_keras_prob_vec
 )
 
 conf_mat(estimates_keras_tbl, truth, estimate)
 accuracy(estimates_keras_tbl, truth, estimate)
 roc_auc(estimates_keras_tbl, truth, class_prob)
 precision(estimates_keras_tbl, truth, estimate)
 recall(estimates_keras_tbl, truth, estimate)
 
 
 explainer <- lime::lime(
   x_train_tbl, 
   model = model_keras, 
   bin_continuous = FALSE
 )
 
 explanation <- lime::explain(
   x_test_tbl[1:10,], 
   explainer = explainer, 
   n_labels = 1, 
   n_features = 4, 
   kernel_width = 0.5
 )
 
 plot_features(explanation)
 plot_explanations(explanation)
 
 
 
 corrr_analysis <- x_train_tbl %>%
   mutate(Churn = y_train_vec) %>%
   correlate() %>%
   focus(Churn) %>%
   rename(feature = rowname) %>%
   arrange(abs(Churn)) %>%
   mutate(feature = as_factor(feature))
 
 corrr_analysis %>%
   ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
   geom_point() +
   geom_segment(aes(xend = 0, yend = feature), color = "red", data = corrr_analysis %>% filter(Churn > 0.25)) +
   geom_point(color = "red", data = corrr_analysis %>% filter(Churn > 0.25)) +
   geom_segment(aes(xend = 0, yend = feature), color = "#2DC6D6", data = corrr_analysis %>% filter(Churn < -0.25)) +
   geom_point(color = "#2DC6D6", data = corrr_analysis %>% filter(Churn < -0.25)) +
   geom_vline(xintercept = 0, color = "#f1fa8c", size = 1, linetype = 2) +
   labs(title = "Feature Importance for Churn", x = "Correlation with Churn", y = "Feature")
 

   

  