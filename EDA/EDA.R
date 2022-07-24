library(tidyverse)
library(tidymodels)
set.seed(1)
bankruptcy <- read_csv("data/data.csv") %>%
  janitor::clean_names()

skimr::skim(bankruptcy)
# no missing data

# split data 
bankruptcy_split <- initial_split(bankruptcy, prop = 0.8, strata = bankrupt)
bankruptcy_train <- training(bankruptcy_split)
bankruptcy_test <- testing(bankruptcy_split)

bankruptcy_EDA <- initial_split(bankruptcy_train, prop = 0.5, strata = bankrupt)
EDA_train <- training(bankruptcy_EDA)
EDA_test <- testing(bankruptcy_EDA)


# evaluation of outcome variable
EDA_train %>%
  ggplot(mapping = aes(x = bankrupt)) +
  geom_bar()
# heavy skew towards non-bankrupt

# evaluation of roa_c_before_interest_and_depreciation_before_interest
EDA_train %>%
  ggplot(mapping = aes(x = roa_c_before_interest_and_depreciation_before_interest)) +
  geom_histogram()

# evaluation of roa_a_before_interest_and_percent_after_tax
EDA_train %>%
  ggplot(mapping = aes(x = roa_a_before_interest_and_percent_after_tax)) +
  geom_histogram()
# slight left skew

# evaluation of roa_b_before_interest_and_depreciation_after_tax
EDA_train %>%
  ggplot(mapping = aes(x = roa_b_before_interest_and_depreciation_after_tax)) +
  geom_histogram()
# slight left skew

# evaluation of operating_gross_margin
EDA_train %>%
  ggplot(mapping = aes(x = operating_gross_margin)) +
  geom_histogram()
# most values concentrated between 0.5 and 0.75

# evaluation of realized_sales_gross_margin
EDA_train %>%
  ggplot(mapping = aes(x = realized_sales_gross_margin)) +
  geom_histogram()
# most values concentrated between 0.5 and 0.75

# evaluation of operating_profit_rate
EDA_train %>%
  ggplot(mapping = aes(x = operating_profit_rate)) +
  geom_histogram()
# most values are approximately 1

# evaluation of pre_tax_net_interest_rate
EDA_train %>%
  ggplot(mapping = aes(x = pre_tax_net_interest_rate)) +
  geom_histogram()
# values are concentrated around 0.79

# evaluation of after_tax_net_interest_rate
EDA_train %>%
  ggplot(mapping = aes(x = after_tax_net_interest_rate)) +
  geom_histogram()
# most values 0.8

# evaluation of non_industry_income_and_expenditure_revenue
EDA_train %>%
  ggplot(mapping = aes(x = non_industry_income_and_expenditure_revenue)) +
  geom_histogram()
# most values are approximately 0.3

# evaluation of continuous_interest_rate_after_tax
EDA_train %>%
  ggplot(mapping = aes(x = continuous_interest_rate_after_tax)) +
  geom_histogram()
# most values are approximately 0.77

# evaluation of operating_expense_rate
EDA_train %>%
  ggplot(mapping = aes(x = operating_expense_rate)) +
  geom_histogram()
# heavily right skewed, needs a log transformation
EDA_train %>%
  mutate(log_operating_expense_rate = log(operating_expense_rate)) %>%
  ggplot(mapping = aes(x = log_operating_expense_rate)) +
  geom_histogram()
# log transformation does not help

# evaluation of research_and_development_expense_rate
EDA_train %>%
  ggplot(mapping = aes(x = research_and_development_expense_rate)) +
  geom_histogram()
# right skewed, needs a log transformation
EDA_train %>%
  mutate(log_research_and_development_expense_rate = log(research_and_development_expense_rate)) %>%
  ggplot(mapping = aes(x = log_research_and_development_expense_rate)) +
  geom_histogram()
# log transformation does not help

# evaluation of cash_flow_rate
EDA_train %>%
  ggplot(mapping = aes(x = cash_flow_rate)) +
  geom_histogram()

# evaluation of interest_bearing_debt_interest_rate
EDA_train %>%
  ggplot(mapping = aes(x = interest_bearing_debt_interest_rate)) +
  geom_histogram()
# most values approximately 0

# evaluation of tax_rate_a
EDA_train %>%
  ggplot(mapping = aes(x = tax_rate_a)) +
  geom_histogram()
# right skew, needs a log transformation
EDA_train %>%
  mutate(log_tax_rate_a = log(tax_rate_a)) %>%
  ggplot(mapping = aes(x = log_tax_rate_a)) +
  geom_histogram()
# some left skew after transformation but better

# evaluation of net_value_per_share_b
EDA_train %>%
  ggplot(mapping = aes(x = net_value_per_share_b)) +
  geom_histogram()
# some right skew

# evaluation of net_value_per_share_a
EDA_train %>%
  ggplot(mapping = aes(x = net_value_per_share_a)) +
  geom_histogram()

# evaluation of net_value_per_share_c
EDA_train %>%
  ggplot(mapping = aes(x = net_value_per_share_c)) +
  geom_histogram()

# evaluation of persistent_eps_in_the_last_four_seasons
bankruptcy %>%
  ggplot(mapping = aes(x = persistent_eps_in_the_last_four_seasons)) +
  geom_histogram()

# evaluation of cash_flow_per_share
EDA_train %>%
  ggplot(mapping = aes(x = cash_flow_per_share)) +
  geom_histogram()

# evaluation of revenue_per_share_yuan
EDA_train %>%
  ggplot(mapping = aes(x = revenue_per_share_yuan)) +
  geom_histogram()
# all values are approximately 0

# evaluation of operating_profit_per_share_yuan
EDA_train %>%
  ggplot(mapping = aes(x = operating_profit_per_share_yuan)) +
  geom_histogram()

# evaluation of per_share_net_profit_before_tax_yuan
EDA_train %>%
  ggplot(mapping = aes(x = per_share_net_profit_before_tax_yuan)) +
  geom_histogram()

# evaluation of realized_sales_gross_profit_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = realized_sales_gross_profit_growth_rate)) +
  geom_histogram()
# values concentrated around 0.2

# evaluation of operating_profit_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = operating_profit_growth_rate)) +
  geom_histogram()
# values concentrated around 0

# evaluation of after_tax_net_profit_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = after_tax_net_profit_growth_rate)) +
  geom_histogram()
# values concentrated around 0.68

# evaluation of regular_net_profit_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = regular_net_profit_growth_rate)) +
  geom_histogram()
# values concentrated around 0.68

# evaluation of continuous_net_profit_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = continuous_net_profit_growth_rate)) +
  geom_histogram()
# values concentrated around 0.21

# evaluation of total_asset_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = total_asset_growth_rate)) +
  geom_histogram()
# weird second distribution near 0

# evaluation of net_value_growth_rate
EDA_train %>%
  ggplot(mapping = aes(x = net_value_growth_rate)) +
  geom_histogram()
# values concentrated near 0

# evaluation of total_asset_return_growth_rate_ratio
EDA_train %>%
  ggplot(mapping = aes(x = total_asset_return_growth_rate_ratio)) +
  geom_histogram()
# values concentrated near 0.26

# evaluation of cash_reinvestment_percent
EDA_train %>%
  ggplot(mapping = aes(x = cash_reinvestment_percent)) +
  geom_histogram()

# evaluation of current_ratio
EDA_train %>%
  ggplot(mapping = aes(x = current_ratio)) +
  geom_histogram()
# values clustered near 0

# evaluation of quick_ratio
EDA_train %>%
  ggplot(mapping = aes(x = quick_ratio)) +
  geom_histogram()
# all values are approximately 0

# evaluation of interest_expense_ratio
EDA_train %>%
  ggplot(mapping = aes(x = interest_expense_ratio)) +
  geom_histogram()
# values concentrated near 0.62

# evaluation of total_debt_total_net_worth
EDA_train %>%
  ggplot(mapping = aes(x = total_debt_total_net_worth)) +
  geom_histogram()
# all values are approximately 0

# evaluation of debt_ratio_percent
EDA_train %>%
  ggplot(mapping = aes(x = debt_ratio_percent)) +
  geom_histogram()

# evaluation of net_worth_assets
EDA_train %>%
  ggplot(mapping = aes(x = net_worth_assets)) +
  geom_histogram()
# slight left skew

# evaluation of long_term_fund_suitability_ratio_a
EDA_train %>%
  ggplot(mapping = aes(x = long_term_fund_suitability_ratio_a)) +
  geom_histogram()
# values clusted near 0

# evaluation of borrowing_dependency
EDA_train %>%
  ggplot(mapping = aes(x = borrowing_dependency)) +
  geom_histogram()
# values clustered near 0.36

# evaluation of contingent_liabilities_net_worth
EDA_train %>%
  ggplot(mapping = aes(x = contingent_liabilities_net_worth)) +
  geom_histogram()
# all values are approximately 0

# evaluation of operating_profit_paid_in_capital
EDA_train %>%
  ggplot(mapping = aes(x = operating_profit_paid_in_capital)) +
  geom_histogram()

# evaluation of net_profit_before_tax_paid_in_capital
EDA_train %>%
  ggplot(mapping = aes(x = net_profit_before_tax_paid_in_capital)) +
  geom_histogram()

# evaluation of inventory_and_accounts_receivable_net_value
EDA_train %>%
  ggplot(mapping = aes(x = inventory_and_accounts_receivable_net_value)) +
  geom_histogram()
# values concentrated around 0.39, slight right skew

# evaluation of total_asset_turnover
EDA_train %>%
  ggplot(mapping = aes(x = total_asset_turnover)) +
  geom_histogram()
# right skew, needs log transformation
EDA_train %>%
  mutate(log_total_asset_turnover = log(total_asset_turnover)) %>%
  ggplot(mapping = aes(x = log_total_asset_turnover)) +
  geom_histogram()
# still some skew but better

# evaluation of accounts_receivable_turnover
EDA_train %>%
  ggplot(mapping = aes(x = accounts_receivable_turnover)) +
  geom_histogram()
# all values are approximately 0

# evaluation of average_collection_days
EDA_train %>%
  ggplot(mapping = aes(x = average_collection_days)) +
  geom_histogram()
# all values are approximately 0

# evaluation of inventory_turnover_rate_times
EDA_train %>%
  ggplot(mapping = aes(x = inventory_turnover_rate_times)) +
  geom_histogram()
# heavy right skew, needs log transformation
EDA_train %>%
  mutate(log_inventory_turnover_rate_times = log(inventory_turnover_rate_times)) %>%
  ggplot(mapping = aes(x = log_inventory_turnover_rate_times)) +
  geom_histogram()
# transformation does not help

# evaluation of fixed_assets_turnover_frequency 
EDA_train %>%
  ggplot(mapping = aes(x = fixed_assets_turnover_frequency)) +
  geom_histogram()
# heavy right skew, needs log transformation
EDA_train %>%
  mutate(log_fixed_assets_turnover_frequency = log(fixed_assets_turnover_frequency)) %>%
  ggplot(mapping = aes(x = log_fixed_assets_turnover_frequency)) +
  geom_histogram()
# log transformation does not help


