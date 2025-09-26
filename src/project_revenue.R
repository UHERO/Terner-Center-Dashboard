#=========================================================
# Project Revenue R Script:
#
# Calculates project revenue & income metrics: PGI, EGI, NOI
# Also calculates operating costs (post-construction)
#
# @author: Sakura
# @date: 9/26/25
#=========================================================

#=========================================================
# This is for PGI, EGI, total operating expenses, etc. to calculate NOI
#=========================================================

# load cost functions from assumption parameters
source("C:/Users/1saku/Desktop/Housing/src/Assumptions.R")

# load cost functions from construction costs script
source("C:/Users/1saku/Desktop/Housing/src/project_costs.R")

# function to calculate PRI (potential rental income)
calc_pri <- function(max_units, affordable_units, assumptions) {
  regular_market_rent <- assumptions$current_unit$mkt_rent_monthly
  afford_market_rent <- assumptions$current_unit$afd_rent_monthly
  
  # calculates annual PRI 
  pri <- (regular_market_rent * (max_units - affordable_units) * 12) +
    (afford_market_rent * affordable_units * 12)
  return(pri)
}

# function to calculate annual parking income
calc_parking_income <- function(max_units, assumptions) {
  num_parking_spaces <- max_units * assumptions$parking$spaces_per_unit
  parking_income <- num_parking_spaces * assumptions$parking$park_rate * 12
  return(parking_income)
}

# function to calculate management fees 
# uses the PRI value (which becomes yearly_rent parameter)
calc_mgmt_fee <- function(yearly_rent, assumptions) {
  return(yearly_rent * assumptions$expenses$property_mgmt$fee_rate)
}

# function to calculate admin income per unit
calc_admin_income_per_unit <- function(assumptions) {
  return(assumptions$income$apt_admin$calc_annual_admin())
}

# annual admin income for all units
admin_income_per_unit <- calc_admin_income_per_unit(assumptions)

# applies calculations to construction_costs_df
operating_costs_df <- construction_costs_df %>%
  mutate(
    # income calculations
    PRI = calc_pri(max_units, affordable_units, assumptions),
    parking_income = calc_parking_income(max_units, assumptions),
    admin_income = admin_income_per_unit * max_units,
    total_other_income = parking_income + admin_income,
    PGI = PRI + total_other_income,  # Potential Gross Income
    
    # vacancy & credit loss (for rent)
    vacancy_loss = PGI * assumptions$vacancy$rental,
    
    # EGI (effective gross income)
    EGI = PGI - vacancy_loss,  
    
    # operating expenses
    mgmt_fee = calc_mgmt_fee(PRI, assumptions),
    property_insurance = assumptions$expenses$insurance$property_per_sqft * buildable_sqft,
    liability_insurance = assumptions$expenses$insurance$liability_per_unit * max_units,
    total_operating_expenses = mgmt_fee + property_insurance + liability_insurance,
    
    # NOI (net operating income)
    NOI = EGI - total_operating_expenses
  )
