library(tibble)
library(dplyr)

# Create the global variables as df
globals_df <- tribble(
  ~category,              ~variable,              ~value,
  
  # Construction costs ($/sqft)
  "Construction Costs",   "cost_lowrise_wood",    170.50,
  "Construction Costs",   "cost_midrise_concrete",161.21,
  "Construction Costs",   "cost_highrise_concrete",266.91,
  
  # FAR limits
  "Zoning",               "FAR_BMX3",             2.5,
  "Zoning",               "FAR_BMX4",             4.0,
  
  # Finance
  "Finance",              "loan_to_cost",         0.70,
  "Finance",              "preferred_return",     0.08,
  "Finance",              "interest_rate",        0.067,
  
  # Market
  "Market",               "vacancy_rate",         0.126,
  "Market",               "rental_vacancy_rate",  0.074,
  
  # Timeline 
  "Timeline",             "months_lowrise",       12,
  "Timeline",             "months_midrise",       18,
  "Timeline",             "months_highrise",      30
)

# Turn into a named vector for ease
globals_vec <- setNames(globals_df$value, globals_df$variable)
