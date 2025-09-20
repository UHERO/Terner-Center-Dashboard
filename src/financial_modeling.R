#=========================================================
# Financial Modeling R Script:
#
# Creates a new df with construction costs & land value.
#
# @author: Sakura
# @date: 9/19/25
#=========================================================

# NOTES:
# the csv is from BFS Real Property Assessment on HIGIS called "ASMTGIS-Table",
# however the csv is updated weekly so the data may not be accurate.
# there exists multiple tmk rows in the csv possibly due to multiple assessment units on a parcel,
# to mitigate this, we take the avg land value of the repeating tmks.

# load library
library(dplyr)

# read CSV and clean & aggregate land value
land_value_df <- read.csv("C:/Users/1saku/Desktop/Housing/data/ASMTGIS_Table.csv", stringsAsFactors = FALSE) %>%
  select(tmk, landvalue) %>%      
  rename(
    cty_tmk = tmk,
    land_value = landvalue
  ) %>%
  group_by(cty_tmk) %>%            
  summarise(
    avg_land_value = mean(land_value, na.rm = TRUE)  
  ) %>%
  ungroup()

# merge two dfs by cty_tmk 
financial_df <- construction_costs_df %>% 
  left_join(land_value_df, by = "cty_tmk") 


