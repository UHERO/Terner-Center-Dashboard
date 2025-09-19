#=========================================================
# Construction Costs R Script:
#
# Creates a new df with FAR, hard + soft costs, total cost
# Keeps tmk, city_tmk, zone_class from 'parzon'
# @author: Sakura
# @date: 9/19/25
#=========================================================

# libraries
library(sf)
library(purrr)
library(dplyr)
library(tidyr)

# GLOBAL VARIABLES
# need to change soft cost % & maybe add permit fees per sqft?
soft_pct   <- 0.3


# construction cost table
construction_costs <- data.frame(
  zone_class  = c("A-1", "A-2", "A-3", "AMX-1", "AMX-2", "AMX-3"),
  cost_per_sf = c(170.50, 161.21, 183.70, 200.00, 266.91, 231.18),
  stringsAsFactors = FALSE
)


# FAR calculation
calc_far <- function(zone, lot_sqft) {
  if (zone %in% c("A-1", "AMX-1")) {
    if (lot_sqft < 10000) return((0.00003 * lot_sqft) + 0.3)
    if (lot_sqft <= 40000) return((0.00001 * lot_sqft) + 0.5)
    return(0.9)
  }
  if (zone %in% c("A-2", "AMX-2")) {
    if (lot_sqft < 10000) return((0.00009 * lot_sqft) + 0.4)
    if (lot_sqft <= 40000) return((0.00002 * lot_sqft) + 1.1)
    return(1.9)
  }
  if (zone %in% c("A-3", "AMX-3")) {
    if (lot_sqft < 10000) return((0.00014 * lot_sqft) + 0.6)
    if (lot_sqft <= 20000) return((0.00004 * lot_sqft) + 1.6)
    if (lot_sqft <= 40000) return((0.00002 * lot_sqft) + 2.0)
    return(2.8)
  }
  warning(paste("Zone not recognized for FAR:", zone))
  return(NA)
}

# calculates various construction costs
calc_construction_costs <- function(zone, lot_sqft) {
  rate <- construction_costs$cost_per_sf[construction_costs$zone_class == zone]
  
  if (length(rate) == 0) {
    warning(paste("Zone class not recognized:", zone))
    return(c(FAR = NA, buildable_sqft = NA, hard_cost = NA, soft_cost = NA, total_estimated_cost = NA))
  }
  # functions for buildable sqft, hard cost, soft cost, & total construction cost
  far <- calc_far(zone, lot_sqft)
  buildable_sqft <- lot_sqft * far
  hard_cost <- buildable_sqft * rate
  soft_cost <- hard_cost * soft_pct
  total_estimated_cost <- hard_cost + soft_cost
  
  return(c(FAR = far, buildable_sqft = buildable_sqft,
           hard_cost = hard_cost, soft_cost = soft_cost,
           total_estimated_cost = total_estimated_cost))
}



