#=========================================================
# Project Costs R Script:
#
# The pathways are specific to my local drive so feel free to change them to run it.
# Calculates the total project costs: land, hard, soft
# Also calculates max & affordable units for TOD & non-TOD parcels
# @author: Sakura
# @date: 9/26/25
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

# loads in TOD boundary zones 
tod_sf <- st_read("C:/Users/1saku/Desktop/Housing/data/TOD_Boundary/TOD_Plan_Boundary.shp")

# change CRS of tod_sf to match parzon
tod_sf <- st_transform(tod_sf, st_crs(parzon))

# creates TOD column (0=no, 1=yes)
parzon$TOD <- as.numeric(lengths(st_intersects(parzon, tod_sf)) > 0)

# parzon is a sf object, drop geometry column, maybe attach it later
parzon_df <- st_drop_geometry(parzon) 

# function to get FAR with TOD bonus
# applying all TOD zones with (max FAR w/ minor special district permit (21.9 100-8))
calc_far <- function(zone, lot_sqft, tod_status, assumptions) {
  base_far <- NA
  if(zone %in% c("A-1","AMX-1")) base_far <- assumptions$density$FAR_table$A1_AMX1(lot_sqft)
  if(zone %in% c("A-2","AMX-2")) base_far <- assumptions$density$FAR_table$A2_AMX2(lot_sqft)
  if(zone %in% c("A-3","AMX-3")) base_far <- assumptions$density$FAR_table$A3_AMX3(lot_sqft)
  
  if(is.na(base_far)) {
    warning(paste("Unknown zone:", zone))
    return(NA)
  }
  
  # Apply TOD bonus if parcel is in TOD area
  if(tod_status == 1) {
    base_far <- base_far * 1.2
  }
  
  return(base_far)
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
  
  # NEED TO ADD: 
  # how parking plays a role into total buildable sqft for apartment units
  # buildable sqft
  far <- calc_far(zone, lot_sqft, parcel_row$TOD, assumptions)
  buildable_sqft <- lot_sqft * far
  
  # max units
  max_units <- floor(buildable_sqft / assumptions$current_unit$size_sqft)
  
  # need to implement inclusionary housing (ie. Transit Project Rates)
  # inclusionary housing placeholder, perhaps create a function in assumptions?
  # for now using the for rent affordable unit share rate
  affordable_units <- round(max_units * 0.05)  
  
  # construction costs
  hard_cost <- buildable_sqft * rate
  soft_cost <- assumptions$construction$soft_cost_fn(hard_cost, parcel_row)
  total_construct_cost <- hard_cost + soft_cost
  
  return(c(
    buildable_sqft = buildable_sqft,
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

#=========================================================
# Total Development Cost: 
# This gives land cost estimate
# For later on in the initial investment in developer's ROR & total dev cost
#=========================================================

#=========================================================
# NOTES:
# the csv is from BFS Real Property Assessment on HIGIS called "ASMTGIS-Table",
# however the csv is updated weekly so the data may not be accurate.
# there exists multiple tmk rows in the csv possibly due to multiple assessment units on a parcel,
# to mitigate this, we take the avg land value of the repeating tmks.
#========================================================= 

# # load library
# library(dplyr)
#
# # read CSV and clean & aggregate land value
# land_value_df <- read.csv("C:/Users/1saku/Desktop/Housing/Terner-Center-Dashboard/data/ASMTGIS_Table.csv", stringsAsFactors = FALSE) %>%
#   select(tmk, landvalue) %>%
#   rename(
#     cty_tmk = tmk,
#     land_value = landvalue
#   ) %>%
#   group_by(cty_tmk) %>%
#   summarise(
#     avg_land_value = mean(land_value, na.rm = TRUE)
#   ) %>%
#   ungroup()
#
# # merge two dfs by cty_tmk
# financial_df <- construction_costs_df %>%
#   left_join(land_value_df, by = "cty_tmk")