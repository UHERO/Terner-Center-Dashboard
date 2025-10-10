#=========================================================
# Project Costs R Script:
#
# The pathways are specific to my local drive so feel free to change them to run it.
# Calculates the total project costs: land, hard, soft
# Also calculates max & affordable units for TOD & non-TOD parcels
# @author: Sakura
# @date: 10/10/25
#=========================================================

#=========================================================
# Global Variables: 
#=========================================================

# change this to select the sale type (for sale vs for rent)
SELECTED_SALE_TYPE <- "for_rent" 

sale_type <- list( 
  for_rent = NA,
  for_sale = NA
  )

# change this to select the affordable unit share 
SELECTED_TOD_AFD <- "tod_15"
SELECTED_OTHER_AFD <- "other_15"

project_type <- list(
  # all other projects afford unit % (for sale)
  other_15 = 0.15, 
  other_20 = 0.2,
  # TOD project afford unit % (for sale)
  tod_30 = 0.3, 
  tod_35 = 0.35  
)

#=========================================================
# libraries
library(dplyr)
library(tidyr)

# load cost functions from assumption parameters
source("C:/Users/1saku/Desktop/Housing/src/assumptions.R")

# load cleaned data from cleaning_data R script
source("C:/Users/1saku/Desktop/Housing/src/data_cleaning.R") 
#=========================================================

# function to get FAR with TOD & Special District bonus
calc_far <- function(zone_class, lot_sqft, TOD_adopt, TOD_proposed, Special_zone, density) {
  base_far <- NA
  if(zone_class %in% c("A-1","AMX-1")) base_far <- density$FAR_table$A1_AMX1(lot_sqft)
  if(zone_class %in% c("A-2","AMX-2")) base_far <- density$FAR_table$A2_AMX2(lot_sqft)
  if(zone_class %in% c("A-3","AMX-3")) base_far <- density$FAR_table$A3_AMX3(lot_sqft)

  if(is.na(base_far)) {
    warning(paste("Unknown zone:", zone_class))
    return(NA)
  }
  if(TOD_adopt == 1 | TOD_proposed == 1) {
    base_far <- base_far * 2
  }
  if(Special_zone == 1) {
    base_far <- base_far * 1.2
  } 
  return(base_far)
}

# soft cost function (only includes building permit & regulatory for now)
calc_soft_costs <- function(hard_cost, parcel_row, num_units = NULL) {
  
  total_soft_cost <- 0
  
  # building permit fee
  permit_fee <- fees$building_permit(hard_cost)
  total_soft_cost <- total_soft_cost + permit_fee
  
  # plan review fee (10% of permit fee or $200, whichever is greater)
  plan_review_fee <- fees$plan_review_revision(permit_fee)
  total_soft_cost <- total_soft_cost + plan_review_fee
  
  # administrative fees 
  admin_fees <- fees$administrative$material_methods_application +
    fees$administrative$temp_occupancy_cert +
    fees$administrative$third_party_cert_initial
  total_soft_cost <- total_soft_cost + admin_fees
  
  # impact fees (edit when found)
  if (!is.null(num_units) && !is.na(fees$impact$per_unit)) {
    impact_fees <- num_units * fees$impact$per_unit
    total_soft_cost <- total_soft_cost + impact_fees
  }
  
  return(total_soft_cost)
}

# total construction cost function (hard & soft)
calc_construction_costs <- function(zone_class, lot_sqft, parcel_row, construction, density) {
  # hard cost rate
  rate <- construction$cost_per_zone$cost_per_sf[
    construction$cost_per_zone$zone_class == zone_class
  ]
  
  if (length(rate) == 0) {
    warning(paste("Unknown zone:", zone_class))
    return(data.frame(
      buildable_sqft = NA, 
      max_units = NA, 
      affordable_units = NA,
      hard_cost = NA, 
      soft_cost = NA, 
      construct_cost = NA
    ))
  }
  
  # buildable sqft
  # (add in height function later on)
  far <- calc_far(
    zone_class = zone_class, 
    lot_sqft = lot_sqft, 
    TOD_adopt = parcel_row$TOD_adopt,
    TOD_proposed = parcel_row$TOD_proposed,
    Special_zone = parcel_row$Special_zone,
    density = density
  )
  buildable_sqft <- lot_sqft * far
  
  # max units
  max_units <- floor(buildable_sqft / current_unit$size_sqft)
  
  # affordable units for TOD & regular parcels
  if (SELECTED_SALE_TYPE == "for_rent") {
    if (parcel_row$TOD_proposed == 1 | parcel_row$TOD_adopt == 1) {
      affordable_units <- round(max_units * 0.15)
    } else {
      affordable_units <- round(max_units * 0.05)
    }
  } else if (SELECTED_SALE_TYPE == "for_sale") {
    if (parcel_row$TOD_proposed == 1 | parcel_row$TOD_adopt == 1) {
      affordable_units <- round(max_units * project_type[[SELECTED_TOD_AFD]])
    } else {
      affordable_units <- round(max_units * project_type[[SELECTED_OTHER_AFD]])
    }
  }
  
  # construction costs
  hard_cost <- buildable_sqft * rate
  
  # soft costs (regulatory fees only)
  soft_cost <- calc_soft_costs(
    hard_cost = hard_cost, 
    parcel_row = parcel_row, 
    num_units = max_units
  )
  
  # total construction cost
  construct_cost <- hard_cost + soft_cost
  
  return(data.frame(
    buildable_sqft = buildable_sqft,
    max_units = max_units,
    affordable_units = affordable_units,
    hard_cost = hard_cost,
    soft_cost = soft_cost,
    construct_cost = construct_cost
  ))
}

# create project_costs df
cost_results <- list()

# loop through each parcel
for (i in 1:nrow(parcels)) {
  parcel <- parcels[i, ]
  
  # calculate construction costs
  costs <- calc_construction_costs(
    zone_class = parcel$zone_class,
    lot_sqft = parcel$lot_sqft,
    parcel_row = parcel,
    construction = construction,
    density = density
  )
  
  # use tmk as identifer
  costs$tmk <- parcel$tmk
  
  # add in land value
  costs$land_cost <- parcel$landvalue
  
  cost_results[[i]] <- costs
}

# combine all results into single df
project_costs <- do.call(rbind, cost_results)

# reorder columns 
project_costs <- project_costs %>%
  select(tmk, max_units, affordable_units, land_cost, hard_cost, soft_cost, construct_cost)

# calculate total development cost (pre-financing)
project_costs <- project_costs %>%
  mutate(
    total_dev_cost = land_cost + construct_cost
  )