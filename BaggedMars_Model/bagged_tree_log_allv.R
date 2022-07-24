# load packages 
library(tidyverse)
library(tidymodels)
library(baguette)

# load data
load("data/bankruptcy_data")
load("train_recipe_log_all.rda")

# tree model
tree_model <- bag_tree() %>%
  set_engine("rpart", times = 25) %>%
  set_mode("classification")

# tree workflow
tree_workflow <- workflow() %>%
  add_recipe(train_recipe_log_all)

# fit training
tree_fit <- tree_workflow %>%
  add_model(tree_model) %>%
  fit(bankruptcy_train)
# save results
save(tree_fit, file = "tree_fit.rda")

load("tree_fit.rda")

tree_fit


# fit to test set
bankruptcy_metric <- metric_set(accuracy)

bankruptcy_prediction <- predict(tree_fit, new_data = bankruptcy_test) %>% 
  bind_cols(bankruptcy_test %>% select(bankrupt)) %>% 
  bankruptcy_metric(truth = bankrupt, estimate = .pred_class)
