#=========================================================
# Assumed Parameters & Variables R Script:
#
# Centralized parameters for pro forma model
# Look at pro forma variables spreadsheet for more info 
# Some variables are not used yet, the variables completely removed are listed
# in the spreadsheet.
#
# @author: Sakura
# @date: 10/10/25
#=========================================================

#=========================================================
# Global Variables: 
#=========================================================
# change this to select different unit type (for rent vs for sale & unit size)
SELECTED_UNIT_TYPE <- "one_bb" 

#=========================================================
# CONSTRUCTION VARIABLES
#=========================================================
construction = list(
    
  # hard construction cost per zone (need to change)
  cost_per_zone = data.frame(
    zone_class  = c("A-1", "A-2", "A-3", "AMX-1", "AMX-2", "AMX-3"),
    cost_per_sf = c(170.50, 161.21, 183.70, 200.00, 266.91, 231.18),
    stringsAsFactors = FALSE
  ),
  
  # CONSTRUCTION PREMIUM VARIABLES
  premiums = list(
    new_construction = NA,
    unit_1  = NA,
    unit_2_4 = NA,
    unit_5_19 = NA,
    unit_20_49 = NA,
    unit_50_plus = NA
  )
)

#=========================================================  
# UNIT PARAMETERS 
# taken using max size of units for affordable rental housing units (32-2.9)
# using for both regular & affordable units
#=========================================================
unit = list(
  unit_size = list(
    # studio 1 bath
    studio = 500,
    # 1 bed 1 bath
    one_bb = 650, 
    # two beds 1 bath
    two_bb = 800,
    # three beds 1.5 bath
    three_bb = 1100, 
    
    # for sale sizes 
    studios = 500,
    one_bbs = 650, 
    two_bbs = 800,
    three_bbs = 1100
  ), 
  
  # market price for regular units per month (for rent)
  # uses 100% AMI, 30% of income
  mkt_unit = list (
    # studio 1 bath
    studio_mkt = 2437,
    # 1 bed 1 bath
    one_bb_mkt = 2611, 
    # two beds 1 bath
    two_bb_mkt = 3132,
    # three beds 1.5 bath
    three_bb_mkt = 3614
  ), 
  
  # affordable unit price per month (for rent) (change later) 
  # uses 80% AMI, 30% of income
  afd_unit = list(
    # studio 1 bath
    studio_afd = 1950,
    # 1 bed 1 bath
    one_bb_afd = 2089, 
    # two beds 1 bath
    two_bb_afd = 2506,
    # three beds 1.5 bath
    three_bb_afd = 3132
  ), 
  
  # market unit price per month (for sale)
  mkt_sale_unit = list(
    # studio 1 bath
    studios_mkt = 300000,
    # 1 bed 1 bath
    one_bbs_mkt = 400000, 
    # two beds 1 bath
    two_bbs_mkt = 600000,
    # three beds 1.5 bath
    three_bbs_mkt = 800000
  ), 
    
  # affordable unit price per month (for rent)  
  afd_sale_unit = list(
    # studio 1 bath
    studios_afd = 180000,
    # 1 bed 1 bath
    one_bbs_afd = 250000, 
    # two beds 1 bath
    two_bbs_afd = 360000,
    # three beds 1.5 bath
    three_bbs_afd = 700000
  )
)

#=========================================================
# FAR / DENSITY PARAMETERS
#=========================================================
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
) 

#=========================================================
# VACANCY VARIABLES
#=========================================================
vacancy = list(
  default = 0.126,
  rental = 0.074,
  home = 0.01
) 

#=========================================================
#AFFORDABILITY VARIABLES
#=========================================================
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
)

#=========================================================
# SOFT COST FEE VARIABLES
# all development-related fees including permits and regulatory
# need to find: impact & zoning fees, environmental & special permits, engineer/architect fees 
# should we add in the good standing part?
#=========================================================
fees = list(
  
  # BUILDING PERMIT FEES (Table 18-A)
  building_permit = function(total_valuation) {
    if (is.na(total_valuation) || total_valuation < 0) {
      warning("Invalid total valuation provided for permit fee calculation")
      return(NA)
    }
    
    fee <- 0
    
    if (total_valuation <= 500) {
      fee <- 20
    } else if (total_valuation <= 1000) {
      fee <- 8 + (total_valuation * 2.50 / 100)
    } else if (total_valuation <= 20000) {
      fee <- 12 + (total_valuation * 2.20 / 100)
    } else if (total_valuation <= 50000) {
      fee <- 82 + ((total_valuation - 20000) * 18 / 1000)
    } else if (total_valuation <= 100000) {
      fee <- 286 + ((total_valuation - 50000) * 14 / 1000)
    } else if (total_valuation <= 500000) {
      fee <- 700 + ((total_valuation - 100000) * 10 / 1000)
    } else if (total_valuation <= 2000000) {
      fee <- 3200 + ((total_valuation - 500000) * 5 / 1000)
    } else {
      fee <- 4300 + ((total_valuation - 2000000) * 4.50 / 1000)
    }
    
    return(fee)
  },
  
  # PLAN REVIEW FEES
  plan_review_revision = function(original_permit_fee) {
    if (is.na(original_permit_fee) || original_permit_fee < 0) {
      warning("Invalid original permit fee for plan review")
      return(NA)
    }
    
    # $200 or 10% of original building permit fee, whichever is greater
    return(max(200, original_permit_fee * 0.10))
  },
  
  # ADMINISTRATIVE FEES 
  # not sure which ones we have to do (user input???)
  administrative = list(
    material_methods_renewal = 100,
    master_tract_review = 500,
    contractor_change = 50,
    material_methods_application = 300,
    special_inspection = 1000,
    temp_occupancy_cert = 200,
    third_party_cert_initial = 500,
    third_party_cert_renewal = 1000
  ),
  
  # IMPACT FEES (edit)
  impact = list(
    per_unit = NA,
    per_sqft = NA,
    school_fee = NA,
    park_fee = NA,
    transportation_fee = NA
  ),
  
  # INCLUSIONARY HOUSING IN-LIEU FEES (the fines)
  inclusionary = list(
    in_lieu_per_unit = NA
  )
)

# REGULATORY VARIABLES (edit these are ones from terner center)
regulatory = list(
  fee_per_sf   = NA,
  fee_per_unit = NA,
  fee_per_dollar = NA,
  lump_sum     = NA,
  sfa_dot_fees = c(600, 1000)
)

#=========================================================
# TIME RELATED VARIABLES
#=========================================================
timing = list(
  months_construction = list(
    low_rise = c(12, 18),
    mid_rise = c(18, 24),
    # use 36+ if open-ended
    high_rise = c(24, 36)  # use 36+ if open-ended
  ),
  absorption_units_per_month = NA,
  stable_months_at_sale = NA
)

#=========================================================
# FINANCE VARIABLES (need to change)
#=========================================================
financing = list(
  loan = list(
    loan_to_cost = 0.65,
    # annual interest rate
    interest_rate = 0.045,
    cap_rate_sale = NA,
    preferred_return = NA,
    apperciation = NA
  ), 
  # fees for financing costs
  finance_fees = list(
    impact = NA, 
    # bank fee %
    origination = NA
  )
)

#=========================================================
# CAP RATE VARIABLES
#=========================================================
cap_rates = list(
  # look into if cap Rate: Multifamily Metro Mid & High Rise affects TOC zones
  metro = list(
    mid_high_A = c(0.0540, 0.0567),
    mid_high_B = c(0.0545, 0.0574),
    mid_high_C = c(0.0610, 0.0640)
  )
)

#=========================================================
# ENTITLEMENT VARIABLES
#=========================================================
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

#=========================================================
# PARKING VARIABLES (need to change)
#=========================================================
parking = list(
  # parking fee per parking space per month
  park_rate = 100,  
  # average parking spaces per unit
  spaces_per_unit = 1  
)
  
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
)

#=========================================================
# OPERATING EXPENSES VARIABLES
#=========================================================
expenses = list(
  # property management fees
  property_mgmt = list(
    # 8% of monthly rent collected
    fee_rate = 0.08,  
    # 50% gets passed to renters via higher rent
    pass_through_rate = 0.5  
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

#=========================================================
# Allows for selection of unit size type
#=========================================================
current_unit <- list(
  type = SELECTED_UNIT_TYPE,
  size_sqft = unit$unit_size[[SELECTED_UNIT_TYPE]],
  mkt_rent_annual = unit$mkt_unit[[paste0(SELECTED_UNIT_TYPE, "_mkt")]] * 12,
  afd_rent_annual = unit$afd_unit[[paste0(SELECTED_UNIT_TYPE, "_afd")]] * 12,
  mkt_rent_monthly = unit$mkt_unit[[paste0(SELECTED_UNIT_TYPE, "_mkt")]],
  afd_rent_monthly = unit$afd_unit[[paste0(SELECTED_UNIT_TYPE, "_afd")]]
)