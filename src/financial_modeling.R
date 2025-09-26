#=========================================================
# Financial Modeling R Script:
#
# Creates a new df with land value.
# Adds rental income, vacancy loss, operating expenses,
# and NOI to construction_costs_df.
#
# @author: Sakura
# @date: 9/19/25
#=========================================================

#=========================================================
# NOTES:
# the csv is from BFS Real Property Assessment on HIGIS called "ASMTGIS-Table",
# however the csv is updated weekly so the data may not be accurate.
# there exists multiple tmk rows in the csv possibly due to multiple assessment units on a parcel,
# to mitigate this, we take the avg land value of the repeating tmks.
#========================================================= 

#=========================================================
# This is for later on for the initial investment in developer's ROR 
#=========================================================

# load library
library(dplyr)

# read CSV and clean & aggregate land value
land_value_df <- read.csv("C:/Users/1saku/Desktop/Housing/Terner-Center-Dashboard/data/ASMTGIS_Table.csv", stringsAsFactors = FALSE) %>%
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



#=========================================================
# Adds rental income, vacancy loss, operating expenses,
# and NOI to construction_costs_df.
#=========================================================

# load cost functions from assumption parameters
source("C:/Users/1saku/Desktop/Housing/src/Assumptions.R")

calc_income_expenses <- function(construction_costs_df, assumptions) {
  construction_costs_df %>%
    mutate(
      # annual rent potential
      potential_rent_market = max_units * assumptions$affordability$market_rent * 12,
      potential_rent_affordable = affordable_units * assumptions$affordability$affordable_rent * 12,
      potential_rent_total = potential_rent_market + potential_rent_affordable,

      # effective gross income (vacancy adjustment for rent vs for sale)
      # EGI = PGI - vacancy & credit losses
      EGI = potential_rent_total * (1 - assumptions$vacancy$default),

      # operating expenses
      OE = EGI * assumptions$financing$op_ex_ratio,

      # net operating income
      NOI = EGI - OE
    )
}

# income expenses df with PGI, EGI, OE, NOI
income_expenses_df <- calc_income_expenses(construction_costs_df, assumptions)

# potential_rent_market + potential_rent_affordable → PGI → (vacancy) → EGI → (expenses) → NOI → (debt, sale, cash flow) → IRR/NPV.



