#=========================================================
# Project Finance R Script:
#
# Calculates project financing costs: construction loan intrest, financing fees, 
# & equity contributions
# Also calculates construction schedule
# @author: Sakura
# @date: 9/26/25
#=========================================================

# libraries
library(sf)
library(dplyr)
library(tidyr)

# load cost functions from assumption parameters
source("C:/Users/1saku/Desktop/Housing/src/Assumptions.R")

# load cost functions from construction costs script
source("C:/Users/1saku/Desktop/Housing/src/constructioncosts.R")

# load cost functions from financial modeling parameters
source("C:/Users/1saku/Desktop/Housing/src/financial_modeling.R")

# function for construction timeline by zone
calc_construction_months <- function(zone, assumptions) {
  months <- NA
  if(zone %in% c("A-1","AMX-1")) {
    months <- assumptions$timing$months_construction$low_rise
  } else if(zone %in% c("A-2","AMX-2")) {
    months <- assumptions$timing$months_construction$mid_rise
  } else if(zone %in% c("A-3","AMX-3")) {
    months <- assumptions$timing$months_construction$high_rise
  } else {
    warning(paste("Unknown zone for construction timing:", zone))
    months <- assumptions$timing$months_construction$low_rise  # default
  }
  return(months)
}

# function to calculate construction draw schedule
calc_draw_schedule <- function(total_construct_cost, zone, assumptions) {
  months_constr <- calc_construction_months(zone, assumptions)
  draw_schedule <- rep(total_construct_cost / months_constr, months_constr)
  
  return(list(
    months_construction = months_constr,
    monthly_draw = total_construct_cost / months_constr,
    draw_schedule = draw_schedule,
    total_draws = sum(draw_schedule)
  ))
}

#=========================================================
# calculate construction loan amount 
#=========================================================
# total_dev_cost_pre_financing <- land_cost + total_construct_cost + impact_fee + inclusionary_fee
# land fee is okay with avg land value?
# loan_amount <- loan_to_cost * total_dev_cost_pre_financing

