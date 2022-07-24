# load packages 
library(tidyverse)
library(tidymodels)
library(baguette)

tidymodels_prefer()

# load in data and recipe
load("data/bankruptcy_data.rda")
load("train_recipe_log_all.rda")

# mars model
mars_model <- bag_mars() %>%
  set_engine("earth", times = 25) %>%
  set_mode("classification")

# mars workflow
mars_workflow <- workflow() %>%
  add_recipe(train_recipe_log_all)

# fit training
mars_fit <- mars_workflow %>%
  add_model(mars_model) %>%
  fit(bankruptcy_train)
# save results
save(mars_fit, file = "mars_fit.rda")

# load results
load("mars_fit.rda")

# fit to test set
bankruptcy_metric <- metric_set(accuracy)

mars_prediction <- predict(mars_fit, new_data = bankruptcy_test) %>% 
  bind_cols(bankruptcy_test %>% select(bankrupt)) %>% 
  bankruptcy_metric(truth = bankrupt, estimate = .pred_class)

mars_prediction
