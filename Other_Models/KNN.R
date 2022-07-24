library(tidyverse)
library(tidymodels)
library(doParallel)

#setting up model
knn_model <- 
  nearest_neighbor(
    neighbors = tune(),
    mode = "classification"
  )%>%
  set_engine("kknn")

#grid and parameter
knn_params <- parameters(knn_model) 

knn_grid <- grid_regular(knn_params, levels = 5)

knn_workflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(train_recipe)

#initiate
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

#tuning
knn_tuned <- knn_workflow %>%
  tune_grid(train_fold, grid = knn_grid)

write_rds(knn_tuned, "knn_tuned.rds")
knn_tuned <- read_rds("knn_tuned.rds")
#end the parallel
stopCluster(cl)

#THE BEST
show_best(knn_tuned, metric = "accuracy")

rf_workflow_tuned <- rf_workflow %>%
  finalize_workflow(select_best(rf_tuned, metric = "accuracy"))

rf_results <- fit(rf_workflow_tuned, train)
