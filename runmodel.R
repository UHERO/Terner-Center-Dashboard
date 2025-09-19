# ============================
# Runs the Model Script: 
# 
# Loads in all the different R scripts for the models 
# The pathways are specific to my local drive so feel free to change them to run it 
# @author: Sakura 
# @date: 9/19/25
# ============================

# NOTES:
# parzon is a sf object = data frame w/ special geom column

# libraries to work with sf (drops geom)
library(sf)
library(purrr) 
library(dplyr)
library(tidyr)

# loads global variables
source("C:/Users/1saku/Desktop/Housing/src/globalvariables.R")

# loads cost functions
source("C:/Users/1saku/Desktop/Housing/src/constructioncosts.R")

# loads Housing RData parzon data
# this gives you 'parzon': parcels & zoning classes from Emi's work
load(file = "C:/Users/1saku/Desktop/Housing/RData") 

# drops geometry column
parzon_df <- st_drop_geometry(parzon) 

# applies constructioncosts.R
# expands FAR, buildable_sqft, hard_cost, soft_cost, total_estimated_cost
construction_costs_df <- parzon_df %>%
  select(tmk, cty_tmk, zone_class, lot_sqft) %>%
  mutate(costs = pmap(list(zone_class, lot_sqft), calc_construction_costs)) %>%
  unnest_wider(costs)  



