# ============================
# Runs the Model Script: 
# 
# Loads in all the different R scripts for the models 
# The pathways are specific to my local drive so feel free to change them to run it 
# @author: Sakura 
# @date: 9/11/25
# ============================

# NOTES:
# parzon is a sf object = data frame w/ special geom column

# libraries to work with sf (drops geom)
library(sf)
library(purrr) 

# loads global variables
source("C:/Users/1saku/Desktop/Housing/src/globalvariables.R")

# loads cost functions
source("C:/Users/1saku/Desktop/Housing/src/constructioncosts.R")

# loads Housing RData parzon data
# this gives you 'parzon': parcels & zoning classes from Emi's work
load(file = "C:/Users/1saku/Desktop/Housing/RData") 

# drops geometry column
parzon_df <- st_drop_geometry(parzon)

# calculates construction from constructioncosts.R
# pmap passes each row as a list into calc_hard_costs
parzon$hard_cost <- pmap_dbl(parzon_df, ~ calc_hard_costs(list(...)))
parzon$soft_cost <- calc_soft_costs(parzon$hard_cost)


