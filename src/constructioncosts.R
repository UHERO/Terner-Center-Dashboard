#=========================================================
# Construction Costs R Script:
#
# The pathways are specific to my local drive so feel free to change them to run it.
# Creates a new df with hard + soft costs & affordable + max units.
# Keeps tmk, city_tmk, zone_class from 'parzon'.
# @author: Sakura
# @date: 9/19/25
#=========================================================

# libraries
library(sf)
library(dplyr)
library(tidyr)

# load cost functions from assumption parameters
source("C:/Users/1saku/Desktop/Housing/src/Assumptions.R")

# loads RData parzon data
# this gives you 'parzon': parcels & zoning classes from Emi's work
load(file = "C:/Users/1saku/Desktop/Housing/RData") 

# parzon is a sf object, drop geometry column, maybe attach it later
parzon_df <- st_drop_geometry(parzon) 

# function to get FAR 
calc_far <- function(zone, lot_sqft, assumptions) {
  if(zone %in% c("A-1","AMX-1")) return(assumptions$density$FAR_table$A1_AMX1(lot_sqft))
  if(zone %in% c("A-2","AMX-2")) return(assumptions$density$FAR_table$A2_AMX2(lot_sqft))
  if(zone %in% c("A-3","AMX-3")) return(assumptions$density$FAR_table$A3_AMX3(lot_sqft))
  warning(paste("Unknown zone:", zone))
  return(NA)
}

# function to calculate construction costs per parcel
calc_construction_costs <- function(zone, lot_sqft, parcel_row, assumptions) {
  # hard cost rate
  rate <- assumptions$construction$cost_per_zone$cost_per_sf[
    assumptions$construction$cost_per_zone$zone_class == zone
  ]
  
  if(length(rate) == 0) {
    warning(paste("Unknown zone:", zone))
    return(c(FAR=NA, buildable_sqft=NA, max_units=NA,
             hard_cost=NA, soft_cost=NA, total_construct_cost=NA))
  }
  
  # buildable sqft
  far <- calc_far(zone, lot_sqft, assumptions)
  buildable_sqft <- lot_sqft * far
  
  # max units
  max_units <- floor(buildable_sqft / assumptions$construction$average_unit_sqft)
  
  # need to implement inclusionary housing (ie. Transit Project Rates)
  # inclusionary housing placeholder, perhaps create a function in assumptions?
  affordable_units <- round(max_units * 0.05)  
  
  # construction costs
  hard_cost <- buildable_sqft * rate
  soft_cost <- assumptions$construction$soft_cost_fn(hard_cost, parcel_row)
  total_construct_cost <- hard_cost + soft_cost
  
  return(c(
    max_units = max_units,
    affordable_units = affordable_units,
    hard_cost = hard_cost,
    soft_cost = soft_cost,
    total_construct_cost = total_construct_cost
  ))
}

# new df with hard + soft + total costs, & affordable + max units
construction_costs_df <- parzon_df %>%
  rowwise() %>%
  mutate(costs = list(calc_construction_costs(zone_class, lot_sqft, cur_data(), assumptions))) %>%
  ungroup() %>%
  unnest_wider(costs)
