# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info 
load("bt_res.rda")
load("glm_res.rda")
load("mars_res.rda")

# load data
load("data/bankruptcy_data")
load("train_recipe_log_all.rda")

# Create data stack 
banking_stack <- 
  stacks() %>%
  add_candidates(bt_res) %>%
  add_candidates(mars_res) %>%
  add_candidates(glm_res)

# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

# Blend predictions using penalty defined above (tuning step, set seed)
set.seed(3042)

#fixes the blending issue
registerDoSEQ()

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()

#time to get back to work
bankrupt_model_st <-
  banking_stack %>%
  blend_predictions(penalty = blend_penalty)

#explroing

autoplot(bankrupt_model_st)


# Save blended model stack for reproducibility & easy reference (Rmd report)
save(bankrupt_model_st, file = "bankrupt_model_st")

# fit to ensemble to entire training set 
load("bankrupt_model_st")
banking_model_st <-
  bankrupt_model_st %>%
  fit_members()

#assessing the model
bankruptcy_test <- 
  bankruptcy_test %>%
  bind_cols(predict(banking_model_st, .))

bankruptcy_preds <- 
  bankruptcy_test %>%
  select(bankrupt) %>%
  bind_cols(predict(banking_model_st, bankruptcy_test, members = TRUE))

result <- map_dfr(bankruptcy_preds, accuracy, truth = bankrupt, data = bankruptcy_preds) %>%
  mutate(member = colnames(bankruptcy_preds))

save(result, file = "result.rda")

load("C:/Users/jwang/OneDrive/Desktop/Stat 301-3/final-project-standard-deviants/result.rda")

result #see the result

library(tidymodels)
load("bankrupt_model_st")
autoplot(bankrupt_model_st, type = "weights")
