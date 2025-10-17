#=========================================================
# Project Costs R Script 
#
# Calculates total project costs (land, hard, soft)
# and max/affordable units for TOD, Special District, and Regular parcels.
# @author: Sakura
# @date: 10/17/25
#=========================================================

#=========================================================
# Global Variables
#=========================================================
# either "for_sale" or "for_rent"
SELECTED_SALE_TYPE <- "for_rent" 

SELECTED_TOD_AFD <- "tod_15"
SELECTED_OTHER_AFD <- "other_15"

project_type <- list(
  other_15 = 0.15, 
  other_20 = 0.2,
  tod_30 = 0.3, 
  tod_35 = 0.35  
)

#=========================================================
# libraries and sources
library(dplyr)
library(tidyr)

# load cleaned data & cost functions
source("C:/Users/1saku/Desktop/Housing/src/assumptions.R")
source("C:/Users/1saku/Desktop/Housing/src/data_cleaning.R")
#=========================================================
# function to get FAR with bonuses
calc_far <- function(zone_class, lot_sqft, parcel_row, density) {
  base_far <- NA
  
  if (zone_class %in% c("A-1", "AMX-1")) base_far <- density$FAR_table$A1_AMX1(lot_sqft)
  if (zone_class %in% c("A-2", "AMX-2")) base_far <- density$FAR_table$A2_AMX2(lot_sqft)
  if (zone_class %in% c("A-3", "AMX-3")) base_far <- density$FAR_table$A3_AMX3(lot_sqft)
  
  if (is.na(base_far)) {
    warning(paste("Unknown zone:", zone_class))
    return(NA)
  }
  # TOD bonus
  if (parcel_row$TOD_adopt == 1 | parcel_row$TOD_proposed == 1) {
    base_far <- base_far * 2
  }
  # special district bonus
  if (parcel_row$Special_zone == 1) {
    base_far <- base_far * 1.2
  }
  return(base_far)
}

#=========================================================
# Development Cost Subfunctions
#=========================================================
# TOD PARCELS
calc_TOD_costs <- function(zone_class, lot_sqft, parcel_row, construction, density) {
  far <- calc_far(zone_class, lot_sqft, parcel_row, density)
  buildable_sqft <- lot_sqft * far
  # building construction cost rate
  rate <- construction$cost_per_zone$cost_per_sf[
    construction$cost_per_zone$zone_class == zone_class
  ]
  
  # buildable sqft
  # need to implement: height, setback, park dedication, parking
  max_units <- floor(buildable_sqft / current_unit$size_sqft)
  
  # affordable/inclusionary housing for TOD
  if (SELECTED_SALE_TYPE == "for_rent") {
    affordable_units <- round(max_units * 0.15)
  } else if (SELECTED_SALE_TYPE == "for_sale") {
    affordable_units <- round(max_units * project_type[[SELECTED_TOD_AFD]])
  }
  
  # parking costs for TOD (make it structured)
  parking_summary <- parking$calc_parking_cost(buildable_sqft, "structured")
  parking_cost <- parking_summary$total_cost
  
  # hard & soft calculation
  hard_cost <- (buildable_sqft * rate) + parking_cost
  soft_cost <- calc_soft_costs(hard_cost, parcel_row, max_units, affordable_units)
  construct_cost <- hard_cost + soft_cost
  
  return(data.frame(
    tmk = parcel_row$tmk,
    buildable_sqft = buildable_sqft,
    max_units = max_units,
    affordable_units = affordable_units,
    hard_cost = hard_cost,
    parking_cost = parking_cost,
    soft_cost = soft_cost,
    construct_cost = construct_cost
  ))
}

# SPECIAL DISTRICTS PARCELS
calc_special_district_costs <- function(zone_class, lot_sqft, parcel_row, construction, density) {
  far <- calc_far(zone_class, lot_sqft, parcel_row, density)
  buildable_sqft <- lot_sqft * far
  # building construction cost rate
  rate <- construction$cost_per_zone$cost_per_sf[
    construction$cost_per_zone$zone_class == zone_class
  ]
  
  # buildable sqft
  # need to implement: height, setback, park dedication, parking
  max_units <- floor(buildable_sqft / current_unit$size_sqft)
  
  # affordable/inclusionary housing for special districts 
  if (SELECTED_SALE_TYPE == "for_rent") {
    affordable_units <- round(max_units * 0.05)
  } else if (SELECTED_SALE_TYPE == "for_sale") {
    affordable_units <- round(max_units * project_type[[SELECTED_OTHER_AFD]])
  }
  
  # parking costs for special districts (make it surface)
  parking_summary <- parking$calc_parking_cost(buildable_sqft, "surface")
  parking_cost <- parking_summary$total_cost
  
  # hard & soft calculation
  hard_cost <- (buildable_sqft * rate) + parking_cost
  soft_cost <- calc_soft_costs(hard_cost, parcel_row, max_units, affordable_units)
  construct_cost <- hard_cost + soft_cost
  
  return(data.frame(
    tmk = parcel_row$tmk,
    buildable_sqft = buildable_sqft,
    max_units = max_units,
    affordable_units = affordable_units,
    hard_cost = hard_cost,
    parking_cost = parking_cost,
    soft_cost = soft_cost,
    construct_cost = construct_cost
  ))
}

# REGULAR PARCELS
calc_regular_costs <- function(zone_class, lot_sqft, parcel_row, construction, density) {
  far <- calc_far(zone_class, lot_sqft, parcel_row, density)
  buildable_sqft <- lot_sqft * far
  # building construction cost rate
  rate <- construction$cost_per_zone$cost_per_sf[
    construction$cost_per_zone$zone_class == zone_class
  ]
  
  # buildable sqft
  # need to implement: height, setback, park dedication, parking
  max_units <- floor(buildable_sqft / current_unit$size_sqft)
  
  # affordable/inclusionary housing for regular zones
  if (SELECTED_SALE_TYPE == "for_rent") {
    affordable_units <- round(max_units * 0.05)
  } else if (SELECTED_SALE_TYPE == "for_sale") {
    affordable_units <- round(max_units * project_type[[SELECTED_OTHER_AFD]])
  }
  
  # parking costs for regular zones (make it surface)
  parking_summary <- parking$calc_parking_cost(buildable_sqft, "surface")
  parking_cost <- parking_summary$total_cost
  
  # hard & soft costs calculation
  hard_cost <- (buildable_sqft * rate) + parking_cost
  soft_cost <- calc_soft_costs(hard_cost, parcel_row, max_units, affordable_units)
  construct_cost <- hard_cost + soft_cost
  
  return(data.frame(
    tmk = parcel_row$tmk,
    buildable_sqft = buildable_sqft,
    max_units = max_units,
    affordable_units = affordable_units,
    hard_cost = hard_cost,
    parking_cost = parking_cost,
    soft_cost = soft_cost,
    construct_cost = construct_cost
  ))
}

#=========================================================
# Soft Cost Functions
# need to add impact fees & civil engineering & IPDT/PDT fee
#=========================================================
calc_soft_costs <- function(hard_cost, parcel_row, max_units, affordable_units = 0) {
  total_soft_cost <- 0
  
  # building permit fee & plan review fee
  per_unit_cost <- hard_cost / max_units
  permit_fee_per_unit <- fees$building_permit(per_unit_cost)
  plan_review_fee_per_unit <- fees$plan_review_revision(permit_fee_per_unit)
  
  market_units <- max(max_units - affordable_units, 0)
  
  # fee logic by zoning
  if (parcel_row$TOD_adopt == 1 | parcel_row$TOD_proposed == 1) {
    # TOD: affordable units pay only waiver fee
    permit_related_fees <- (market_units * (permit_fee_per_unit + plan_review_fee_per_unit)) +
      (affordable_units * fees$waiver)
  } else {
    # special/regular: same, but no IPDT fee
    permit_related_fees <- (market_units * (permit_fee_per_unit + plan_review_fee_per_unit)) +
      (affordable_units * fees$waiver)
  }
  
  total_soft_cost <- total_soft_cost + permit_related_fees
  
  # administrative fees (flat per parcel)
  admin_fees <- fees$administrative$temp_occupancy_cert 
  total_soft_cost <- total_soft_cost + admin_fees
  

  # park dedication fee(flat per parcel)
  # need to add how much you pay if you say no to park dedication
  park_fee <- fees$park_dedication()
  total_soft_cost <- total_soft_cost + park_fee
  

  # wastewater facility charge (affordable units are waived)
  wastewater_fee <- 0
  if (market_units > 0) {
    wastewater_df <- parcel_row
    wastewater_df$max_units <- market_units
    wastewater_fee <- fees$civil$wastewater(wastewater_df)
  }
  total_soft_cost <- total_soft_cost + wastewater_fee
  
  # civil engineering fees (flat per project)
  escp_fee <- fees$civil$escp
  total_soft_cost <- total_soft_cost + escp_fee
  
  # environmental review fees (EA + EIS) (flat per project)
  env_fee <- fees$environment$ea + fees$environment$eis
  total_soft_cost <- total_soft_cost + env_fee
  
  # planned development fee (flat per project)
  planned_dev_fee <- fees$planned_dev
  total_soft_cost <- total_soft_cost + planned_dev_fee
  
  return(total_soft_cost)
}

# routes the zones to each development subfunction
calc_construction_costs <- function(zone_class, lot_sqft, parcel_row, construction, density) {
  if (parcel_row$TOD_adopt == 1 | parcel_row$TOD_proposed == 1) {
    message(paste("TMK", parcel_row$tmk, "→ TOD parcel"))
    return(calc_TOD_costs(zone_class, lot_sqft, parcel_row, construction, density))
  } else if (parcel_row$Special_zone == 1) {
    message(paste("TMK", parcel_row$tmk, "→ Special District parcel"))
    return(calc_special_district_costs(zone_class, lot_sqft, parcel_row, construction, density))
  } else {
    message(paste("TMK", parcel_row$tmk, "→ Regular parcel"))
    return(calc_regular_costs(zone_class, lot_sqft, parcel_row, construction, density))
  }
}

# loop through parcel rows & compute
cost_results <- list()

for (i in 1:nrow(parcels)) {
  parcel <- parcels[i, ]
  
  costs <- calc_construction_costs(
    zone_class = parcel$zone_class,
    lot_sqft = parcel$lot_sqft,
    parcel_row = parcel,
    construction = construction,
    density = density
  )
  
  costs$tmk <- parcel$tmk
  costs$land_cost <- parcel$landvalue
  cost_results[[i]] <- costs
}

project_costs <- do.call(rbind, cost_results)

project_costs <- project_costs %>%
  select(tmk, max_units, affordable_units, land_cost, hard_cost, soft_cost, construct_cost) %>%
  mutate(total_dev_cost = land_cost + construct_cost)

