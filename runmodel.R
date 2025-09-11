# ============================
# Runs the Model Script: 
# 
# Loads in all the different R scripts for the models 
# The pathways are specific to my local drive so feel free to change them to run it 
# @author: Sakura 
# @date: 9/11/25
# ============================



# Loads global variables
source("C:/Users/1saku/Desktop/Housing/src/globalvariables.R")

# Loads cost functions
source("C:/Users/1saku/Desktop/Housing/src/constructioncosts.R")

# Loads Housing RData parzon data
load("C:/Users/1saku/Desktop/Housing/RData")  # gives you `parzon`

# just an example to calculate costs for the first 5 parcels
parzon$hard_cost <- apply(parzon[ , ], 1, function(row) calc_hard_costs(as.list(row)))
parzon$soft_cost <- calc_soft_costs(parzon$hard_cost)

# quick peek at the df
head(parzon[, c("tmk", "zone_class", "lot_sqft", "hard_cost", "soft_cost")])

