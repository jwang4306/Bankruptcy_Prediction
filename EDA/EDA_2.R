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

#Creates a pdf and print all the density plots to analyze distribution

pdf("EDA.pdf")


for (i in names(EDA_train)) {
  p <- ggplot(EDA_train, aes_string(x = i)) +
    geom_density()
  
  print(p)
}

#Creates a pdf and print all the boxplot to analyze which variables predict best

EDA_train <- EDA_train %>%
  mutate(bankrupt = factor(bankrupt, levels = c(1,0), label = c("Yes", "No") ))

pdf("EDA_Boxplot.pdf")

for (i in names(EDA_train)) {
  plot <- ggplot(EDA_train, aes_string(x = "bankrupt", y = i)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
    
  print(plot)
}

EDA_train %>%
  select(net_value_grown_rate) %>%
  view()

ggplot(EDA_train, aes(x = bankrupt, y = current_ratio)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_y_continuous(limits = c(0, 0.1))

ggplot(EDA_train, aes(x = bankrupt, y = net_value_growth_rate)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_y_continuous(limits = c(0, 0.001))

#log transformation investigation

EDA_train_log <- EDA_train %>%
  select(equity_to_liability, liability_to_equity, 
         net_income_to_stockholders_equity, gross_profit_to_sales,
         no_credit_interval, net_income_to_total_assets,
         liability_assets_flag, current_liability_to_current_assets,
         cash_flow_to_equity, cash_flow_to_liability,
         cash_flow_to_total_assets, equity_to_long_term_liability,
         current_liability_to_equity, current_liability_to_liability,
         cash_turnover_rate, working_capitcal_turnover_rate,
         total_expense_assets, total_income_total_expense,
         retained_earnings_to_total_assets, current_liabilities_equity,
         working_capital_equity, current_liabilities_liability,
         current_liability_to_assets, cash_total_assets,
         quick_assets_total_assets, working_capital_to_total_assets,
         operating_profit_per_person, inventory_turnover_rate_times,
         total_asset_turnover, inventory_and_accounts_receivable_net_value,
         net_profit_before_tax_paid_in_capital, operating_profit_paid_in_capital,
         borrowing_dependency, net_worth_assets, debt_ratio_percent)

EDA_train_log <- log(EDA_train_log)

pdf("EDA_log.pdf")


for (i in names(EDA_train_log)) {
  p <- ggplot(EDA_train_log, aes_string(x = i)) +
    geom_density()
  
  print(p)
}

# evaluating correlations
EDA_train_numeric <- select_if(EDA_train, is.numeric)
res <- cor(EDA_train_numeric)
round(res, 2) %>%
  view() #keep in mind that corrplot() wont't work; names are too long and too many variables to plot

# interactions (cor > 0.5): roa_a_before_interest_and_percent_after_tax:roa_c_before_interest_and_depreciation_before_interest,roa_a_before_interest_and_percent_after_tax:roa_b_before_interest_and_depreciation_after_tax, roa_b_before_interest_and_depreciation_after_tax:roa_c_before_interest_and_depreciation_before_interest, operating_gross_margin:realized_sales_gross_margin, operating_profit_rate:pre_tax_net_interest_rate, operating_profit_rate:after_tax_net_interest_rate, pre_tax_net_interest_rate:after_tax_net_interest_rate, pre_tax_net_interest_rate:non_industry_income_and_expenditure_revenue, after_tax_net_interest_rate:non_industry_income_and_expenditure_revenue, operating_profit_rate:continuous_interest_rate_after_tax, continuous_interest_rate_after_tax:after_tax_net_interest_rate, non_industry_income_and_expenditure_revenue:continuous_interest_rate_after_tax, roa_c_before_interest_and_depreciation_before_interest:net_value_per_share_b, roa_a_before_interest_and_percent_after_tax:net_value_per_share_b, roa_b_before_interest_and_depreciation_after_tax:net_value_per_share_b, roa_c_before_interest_and_depreciation_before_interest:net_value_per_share_a, roa_b_before_interest_and_depreciation_after_tax:net_value_per_share_a, roa_c_before_interest_and_depreciation_before_interest:net_value_per_share_c, roa_a_before_interest_and_percent_after_tax:net_value_per_share_c, roa_b_before_interest_and_depreciation_after_tax:net_value_per_share_c, roa_c_before_interest_and_depreciation_before_interest:persistent_eps_in_the_last_four_seasons, persistent_eps_in_the_last_four_seasons:roa_a_before_interest_and_percent_after_tax, roa_b_before_interest_and_depreciation_after_tax:persistent_eps_in_the_last_four_seasons, roa_c_before_interest_and_depreciation_before_interest:operating_profit_per_share_yuan, operating_profit_per_share_yuan:roa_a_before_interest_and_percent_after_tax, roa_b_before_interest_and_depreciation_after_tax:operating_profit_per_share_yuan, roa_c_before_interest_and_depreciation_before_interest:per_share_net_profit_before_tax_yuan, per_share_net_profit_before_tax_yuan:roa_a_before_interest_and_percent_after_tax,roa_b_before_interest_and_depreciation_after_tax:per_share_net_profit_before_tax_yuan, roa_c_before_interest_and_depreciation_before_interest:operating_profit_paid_in_capital, operating_profit_paid_in_capital:roa_a_before_interest_and_percent_after_tax, roa_b_before_interest_and_depreciation_after_tax:operating_profit_paid_in_capital, roa_c_before_interest_and_depreciation_before_interest:net_profit_before_tax_paid_in_capital, roa_a_before_interest_and_percent_after_tax:net_profit_before_tax_paid_in_capital,roa_b_before_interest_and_depreciation_after_tax:net_profit_before_tax_paid_in_capital, revenue_per_person:after_tax_net_interest_rate, roa_c_before_interest_and_depreciation_before_interest:retained_earnings_to_total_assets, retained_earnings_to_total_assets:roa_a_before_interest_and_percent_after_tax, roa_b_before_interest_and_depreciation_after_tax:retained_earnings_to_total_assets      

#examining the binomial distributions

EDA_train %>%
  filter(bankrupt == 1) %>%
  ggplot(aes(x = log(cash_turnover_rate))) +
  geom_density(fill = 'red')

EDA_train %>%
  filter(bankrupt == 0) %>%
  ggplot(aes(x = log(cash_turnover_rate))) +
  geom_density(fill = 'red')

EDA_train %>%
  filter(bankrupt == 1) %>%
  ggplot(aes(x = log(inventory_turnover_rate_times))) +
  geom_density(fill = 'red')

EDA_train %>%
  filter(bankrupt == 0) %>%
  ggplot(aes(x = log(inventory_turnover_rate_times))) +
  geom_density(fill = 'red')
