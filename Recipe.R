library(tidyverse)
library(tidymodels)

set.seed(1)

bankruptcy <- read_csv("data/data.csv") %>%
  janitor::clean_names() %>%
  mutate(bankrupt = factor(bankrupt, levels = c(1,0), label = c("Yes", "No") ))

# split data 
bankruptcy_split <- initial_split(bankruptcy, prop = 0.8, strata = bankrupt)
bankruptcy_train <- training(bankruptcy_split)
bankruptcy_test <- testing(bankruptcy_split)

train_fold <- vfold_cv(bankruptcy_train, v = 10, repeats = 5)

train_recipe <- recipe(bankrupt ~ ., data = bankruptcy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())%>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) 

train_prep <- prep(train_recipe)
train_baked <- bake(train_prep, bankruptcy_train)

# logged recipe
train_recipe_log_all <- recipe(bankrupt ~ ., data = bankruptcy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())%>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

train_prep <- prep(train_recipe_log_all)
train_baked <- bake(train_prep, bankruptcy_train)

save(train_recipe_log_all, file = "train_recipe_log_all")

# pared down recipe
train_recipe_small <- recipe(bankrupt ~ net_income_to_stockholders_equity + borrowing_dependency + operating_gross_margin + total_income_total_expense, data = bankruptcy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())%>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) 

save(train_recipe_small, file = "train_recipe_small.rda")

train_recipe_small %>%
  prep() %>%
  bake(new_data = NULL)

# pared down recipe with logs
train_recipe_small_log <- recipe(bankrupt ~ net_income_to_stockholders_equity + borrowing_dependency + operating_gross_margin + total_income_total_expense, data = bankruptcy_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_log(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())%>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

save(train_recipe_small_log, file = "train_recipe_small_log.rda")

train_recipe_small_log %>%
  prep() %>%
  bake(new_data = NULL)
