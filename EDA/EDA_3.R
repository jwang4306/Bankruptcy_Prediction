#investigating violin plot with tiny ass difference

library(tidyverse)
library(tidymodels)
library(Hmisc)
library(corrplot)
set.seed(1)

bankruptcy <- read_csv("data/data.csv") %>%
  janitor::clean_names()

# split data 
bankruptcy_split <- initial_split(bankruptcy, prop = 0.8, strata = bankrupt)
bankruptcy_train <- training(bankruptcy_split)
bankruptcy_test <- testing(bankruptcy_split)

bankruptcy_EDA <- initial_split(bankruptcy_train, prop = 0.5, strata = bankrupt)
EDA_train <- training(bankruptcy_EDA)
EDA_test <- testing(bankruptcy_EDA)

EDA_train <- EDA_train %>%
  mutate(bankrupt = factor(bankrupt, levels = c(1,0), label = c("Yes", "No") ))

#God Bless me

ggplot(EDA_train, aes(x = bankrupt, y = interest_bearing_debt_interest_rate)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_y_continuous(limits = c(0, 0.0045))
