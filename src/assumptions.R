#=========================================================
# Assumed Parameters & Variables R Script:
#
# Centralized parameters for pro forma model
# Example: assumptions$construction$low_rise$wood
#
# @author: Sakura
# @date: 9/26/25
#=========================================================

#=========================================================
# NOTES: 
# some variables are not used yet, the variables completely removed are listed
# in the spreadsheet.
#=========================================================

#=========================================================
# Global Variables: 
#=========================================================
# change this to select different unit type
SELECTED_UNIT_TYPE <- "one_bb" 

#=========================================================
# Assumptions made: 
# Look at pro forma variables spreadsheet for more info 
#=========================================================
# organized in nested list for easy access
assumptions <- list(
  
  # CONSTRUCTION VARIABLES
  construction = list(
    
    # hard construction cost per zone, need to change
    cost_per_zone = data.frame(
      zone_class  = c("A-1", "A-2", "A-3", "AMX-1", "AMX-2", "AMX-3"),
      cost_per_sf = c(170.50, 161.21, 183.70, 200.00, 266.91, 231.18),
      stringsAsFactors = FALSE
    ),
  
    # soft cost function: need to implement regulatory/permit/zoning fees
    soft_cost_fn = function(hard_cost, parcel_row) {
      # placeholder percentage
      return(hard_cost * 0.3)
    }
  ),
  
  # UNIT PARAMETERS 
  # taken using max size of units for affordable rental housing units (32-2.9)
  # using for both regular & affordable units
  unit = list(
    unit_size = list(
      # studio 1 bath
      studio = 500,
      # 1 bed 1 bath
      one_bb = 650, 
      # two beds 1 bath
      two_bb = 800,
      # three beds 1.5 bath
      three_bb = 1100
    ), 
    
    # market price for regular units per month (for rent)
    mkt_unit = list (
      # studio 1 bath
      studio_mkt = 1509,
      # 1 bed 1 bath
      one_bb_mkt = 1711, 
      # two beds 1 bath
      two_bb_mkt = 2201,
      # three beds 1.5 bath
      three_bb_mkt = 3731
    ), 
    
    # affordable unit price per month (for rent) (change later) 
    afd_unit = list(
      # studio 1 bath
      studio_afd = 1200,
      # 1 bed 1 bath
      one_bb_afd = 1300, 
      # two beds 1 bath
      two_bb_afd = 1700,
      # three beds 1.5 bath
      three_bb_afd = 3100
    )
  ),
  
  # FAR / DENSITY PARAMETERS
  density = list(
    FAR_table = list(
      A1_AMX1 = function(lot_sqft) {
        if (lot_sqft < 10000) return((0.00003 * lot_sqft) + 0.3)
        if (lot_sqft <= 40000) return((0.00001 * lot_sqft) + 0.5)
        return(0.9)
      },
      A2_AMX2 = function(lot_sqft) {
        if (lot_sqft < 10000) return((0.00009 * lot_sqft) + 0.4)
        if (lot_sqft <= 40000) return((0.00002 * lot_sqft) + 1.1)
        return(1.9)
      },
      A3_AMX3 = function(lot_sqft) {
        if (lot_sqft < 10000) return((0.00014 * lot_sqft) + 0.6)
        if (lot_sqft <= 20000) return((0.00004 * lot_sqft) + 1.6)
        if (lot_sqft <= 40000) return((0.00002 * lot_sqft) + 2.0)
        return(2.8)
      }
    ),
    
    # max dwelling units per acre
    # perhaps use this for probability of being developed?
    max_du_per_acre = list(
      low_rise = 0.9,
      mid_rise = 1.9,
      high_rise = 2.8
    )
  ), 
  
  # CONSTRUCTION PREMIUM VARIABLES
  premiums = list(
    new_construction = NA,
    unit_1  = NA,
    unit_2_4 = NA,
    unit_5_19 = NA,
    unit_20_49 = NA,
    unit_50_plus = NA
  ),
  
  # VACANCY VARIABLES
  vacancy = list(
    default = 0.126,
    rental = 0.074,
    home = 0.01
  ), 
  
  #AFFORDABILITY VARIABLES
  affordability = list(
    
    # affordable rent threshold
    thresholds = list(
      moderate_110ami = NA,
      low_80ami = NA,
      very_low_50ami = NA,
      extreme_30ami = NA
    ),
    
    # affordable rent share for linkage (per sq ft) fee exemption
    linkage_share = list(
      moderate = NA,
      low = NA,
      very_low = NA,
      extreme = NA
    ),
    
    # affordable rent share for TOC
    # perhaps use this for inclusionary housing & soft costs?
    toc_share = list(
      tier1 = list(low=NA, very_low=NA, extreme=NA),
      tier2 = list(low=NA, very_low=NA, extreme=NA),
      tier3 = list(low=NA, very_low=NA, extreme=NA),
      tier4 = list(low=NA, very_low=NA, extreme=NA)
    ),
    
    # low income below-market-rate share of dwelling units
    below_market_unit_share = NA
  ),
  
  # REGULATORY VARIABLES
  regulatory = list(
    fee_per_sf   = NA,
    fee_per_unit = NA,
    fee_per_dollar = NA,
    lump_sum     = NA,
    sfa_dot_fees = c(600, 1000)
  ),
  
  # TIME RELATED VARIABLES
  timing = list(
    months_construction = list(
      low_rise = c(12, 18),
      mid_rise = c(18, 24),
      high_rise = c(24, 36)  # use 36+ if open-ended
    ),
    absorption_units_per_month = NA,
    stable_months_at_sale = NA
  ), 
  
  # FINANCE VARIABLES
  financing = list(
    income = list(
      loan_to_cost = NA,
      interest_rate = NA,
      cap_rate_sale = NA,
      preferred_return = NA,
      apperciation = NA,
      operate_revenue = NA, 
      # 35% of EGI is operating expenses (change)
      op_ex_ratio = 0.35
    ), 
    # PSEUDO CODE (change)
    loan_amount = NA
  ),
  
  # CAP RATE VARIABLES
  cap_rates = list(
    #look into if cap Rate: Multifamily Metro Mid & High Rise affects TOC zones
    metro = list(
      mid_high_A = c(0.0540, 0.0567),
      mid_high_B = c(0.0545, 0.0574),
      mid_high_C = c(0.0610, 0.0640)
    )
  ), 
  
  # ENTITLEMENT VARIABLES
  entitlement = list(
    timeline_months = list(
      units_2_4  = NA,
      units_5_49 = NA,
      units_50_plus = NA
    ),
    added_cost_pct = list(
      units_2_4  = NA,
      units_5_49 = NA,
      units_50_plus = NA
    ),
    unit_reduction_pct = list(
      units_2_4  = NA,
      units_5_49 = NA,
      units_50_plus = NA
    ) 
  ), 
  
  # PARKING VARIABLES (need to change)
  parking = list(
    # parking fee per parking space per month
    park_rate = 100,  
    # average parking spaces per unit
    spaces_per_unit = 1  
  ),
  
  # OTHER INCOME (need to change)
  income = list(
    # application fees, lease renewal fees, etc.
    apt_admin = list(
      application_fee = 50,
      lease_renewal = 200,
      
      # function to calculate total admin fees per unit per year
      calc_annual_admin = function(turnover_rate = 0.3) {
        # assumes 30% turnover rate (new applications)
        # plus lease renewals for remaining 70%
        return((assumptions$income$apt_admin$application_fee * turnover_rate) + 
                 (assumptions$income$apt_admin$lease_renewal * (1 - turnover_rate)))
      }
    )
  ),
  
  # OPERATING EXPENSES VARIABLES
  expenses = list(
    # Property management fees
    property_mgmt = list(
      fee_rate = 0.08,  # 8% of monthly rent collected
      pass_through_rate = 0.5  # 50% gets passed to renters via higher rent
    ),
    
    # insurance costs
    insurance = list(
      # property insurance per sq ft
      property_per_sqft = 0.25,  
      # general liability per unit per year
      liability_per_unit = 150
    ),
    
    # taxes & fees 
    # should use assessed land value or average apartment property value?
    taxes_fees = list(
      property_tax_rate = 0.012
    )
  )
)

# Allows for selection of unit size type
assumptions$current_unit <- list(
  type = SELECTED_UNIT_TYPE,
  size_sqft = assumptions$unit$unit_size[[SELECTED_UNIT_TYPE]],
  mkt_rent_annual = assumptions$unit$mkt_unit[[paste0(SELECTED_UNIT_TYPE, "_mkt")]] * 12,
  afd_rent_annual = assumptions$unit$afd_unit[[paste0(SELECTED_UNIT_TYPE, "_afd")]] * 12,
  mkt_rent_monthly = assumptions$unit$mkt_unit[[paste0(SELECTED_UNIT_TYPE, "_mkt")]],
  afd_rent_monthly = assumptions$unit$afd_unit[[paste0(SELECTED_UNIT_TYPE, "_afd")]]
)
