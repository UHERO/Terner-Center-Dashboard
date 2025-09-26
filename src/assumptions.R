#=========================================================
# Assumed Parameters & Variables R Script:
#
# Centralized parameters for pro forma model
# Example: assumptions$construction$low_rise$wood
#
# @author: Sakura
# @date: 9/20/25
#=========================================================

# NOTES: 
# some variables are not used yet, the variables completely removed are listed
# in the spreadsheet.

# organized in nested list for easy access
assumptions <- list(
  
  # CONSTRUCTION VARIABLES
  construction = list(
    
    # trivial, will be user input later
    average_unit_sqft = 900,
    
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
    below_market_unit_share = NA, 
    
    # cost of market price rent per unit per month (change)
    market_rent = 2500,
    
    # cost of affordable rent per unit per month (change depending on linkage/threshold?)
    affordable_rent = 1200
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
    loan_to_cost = NA,
    interest_rate = NA,
    cap_rate_sale = NA,
    preferred_return = NA,
    apperciation = NA,
    operate_revenue = NA, 
    # 35% of EGI is operating expenses (change)
    op_ex_ratio = 0.35
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
  )
)
