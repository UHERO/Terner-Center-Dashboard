#=========================================================
# Construction Costs R Script:
# 
# Includes hard & soft construction cost estimates for now 
# @author: Sakura 
# @date: 9/11/25
#=========================================================


# Construction cost table for RData Housing parzon
construction_costs <- data.frame(
  type = c("A_low_denisty", "A_mid_denisty", "A_high_density",
           "AMX_low_density", "AMX_mid_density", "AMX_high_density"),
  # need to check these values, just used the ones in global var spreadsheet
  cost_per_sf = c(170.50, 161.21, 183.70, 200.00, 266.91, 231.18),  
  stringsAsFactors = FALSE
)

# Mapping from zoning code to construction type
zone_to_type <- list(
  "A-1" = "A_low_denisty",
  "A-2" = "A_mid_denisty",
  "A-3" = "A_high_density",
  "AMX-1" = "AMX_low_density",
  "AMX-2" = "AMX_mid_density",
  "AMX-3" = "AMX_high_density"
)

# Calculate hard costs for a parcel
calc_hard_costs <- function(parcel_row) {
  zone <- as.character(parcel_row$zone_class)
  
  if (!zone %in% names(zone_to_type)) {
    warning(paste("Zone class not recognized:", zone))
    return(NA)
  }
  
  building_type <- zone_to_type[[zone]]
  rate <- construction_costs$cost_per_sf[construction_costs$type == building_type]
  
  return(parcel_row$lot_sqft * rate)
}

# Calculates soft costs as a % of hard costs 
# Must find an actual ratio, google said 30% is avg across america
calc_soft_costs <- function(hard_cost, pct = 0.3) {
  return(hard_cost * pct)
}
