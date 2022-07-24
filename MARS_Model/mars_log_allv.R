# load packages 
library(tidyverse)
library(tidymodels)
library(earth)


# load data
load("data/bankruptcy_data")
load("train_recipe_log_all.rda")

# support vector machine model
mars_model <- mars(mode = "classification",
                   num_terms = tune(), 
                   prod_degree = tune()) 

# support vector machine workflow
mars_workflow <- workflow() %>%
  add_model(mars_model) %>%
  add_recipe(train_recipe_log_all)

# create grid
mars_params <- parameters(mars_model) 

mars_grid <- grid_regular(mars_params, levels = 5)

# fit model
mars_tuned <- mars_workflow %>%
  tune_grid(train_fold, grid = mars_grid)

# save model 
save(mars_tuned, file = "mars_tuned.rda")

# load in model data
load("mars_tuned.rda")

# assess model parameters
show_best(mars_tuned, metric = "accuracy")

