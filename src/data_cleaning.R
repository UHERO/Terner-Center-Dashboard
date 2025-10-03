#=========================================================
# Data Cleaning R Script:
#
# Cleans various GIS data
# @author: Sakura
# @date: 10/2/25
#=========================================================

# libraries
library(sf)
library(dplyr)
library(tidyr)

#=========================================================
# Creates zoning & tax parcels for Oahu
#=========================================================

# read in oahu zone boundaries
zones <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/All_Zoning/cty_zoning_oah.shp")

# keep only wanted columns and filter to A-1, A-2, A-3, AMX-1, AMX-2, AMX-3
zones_filtered <- zones %>%
  select(ZONE_CLASS, Shape_Leng, Shape_Area, geometry) %>%
  filter(ZONE_CLASS %in% c("A-1", "A-2", "A-3", "AMX-1", "AMX-2", "AMX-3"))

# read in oahu tmk boundaries
oahutmk <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/Oahu_tmk/Tax.shp") 
oahutmk <- st_make_valid(oahutmk) 

# keep only land sqft/acres, geometry, & oahu tmk
oahutmk_filtered <- oahutmk %>%
  select(tmk, rec_area_a, rec_area_s, geometry)

# change crs
zones_filtered <- st_transform(zones_filtered, st_crs(oahutmk_filtered))

# attach zoning info to parcels, only parcels fully inside a zone
oahu_with_zones <- st_join(
  oahutmk_filtered, 
  zones_filtered,
  join = st_within   
)

oahu_with_zones <- oahu_with_zones %>%
  filter(!is.na(ZONE_CLASS))

# Convert rec_area_a (acres) to sq ft and add to rec_area_s
oahu_parzon <- oahu_with_zones %>%
  mutate(
    lot_sqft = rec_area_s + ifelse(!is.na(rec_area_a), rec_area_a * 43560, 0)
  ) %>%
  # remove parcels with 0 lot size
  filter(lot_sqft > 0) %>%
  select(
    tmk,
    lot_sqft,
    zone_class = ZONE_CLASS,
    geometry
  )

#=========================================================
# Creates different columns for adopted & proposed TOD zones in parcels df
#=========================================================

# loads in proposed TOD boundary zones 
tod_proposed <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/TOD_Interim_Boundary/TOD_Plan_Boundary.shp")
# change CRS of tod_proposed to match oahu_parzon
tod_proposed <- st_transform(tod_proposed, st_crs(oahu_parzon))
# creates proposed TOD column (0=no, 1=yes)
TOD_proposed <- as.numeric(lengths(st_intersects(oahu_parzon, tod_proposed)) > 0)

# loads in already instituted TOD boundary zones 
tod_adopt <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/TOD_Adopted_Boundary/TOD_Zoning.shp")
# change CRS of tod_adopt to match oahu_parzon
tod_adopt <- st_transform(tod_adopt, st_crs(oahu_parzon))
# creates TOD_adopt column (0=no, 1=yes)
TOD_adopt <- as.numeric(lengths(st_intersects(oahu_parzon, tod_adopt)) > 0)

parcels_with_adopt <- st_join(oahu_parzon, tod_adopt, join = st_intersects)

# Create new df called parcels, drop geometry and remove tmk column
parcels <- oahu_parzon %>%
  st_drop_geometry() %>%
  mutate(TOD_proposed = TOD_proposed,
         TOD_adopt = TOD_adopt)

#=========================================================
# Cleans BFS data for average land value
#
# NOTES:
# the csv is from BFS Real Property Assessment on HIGIS called "ASMTGIS-Table",
# if suffix is '0000' that is the master for condo/apartment tmks
# there seems to be unneeded tmks (ex. sidewalks that have landvalue = $100)
# need to filter that out but unsure how the criteria should be
#=========================================================

# read in real property assessment CSV
bfs_df <- read.csv("C:/Users/1saku/Desktop/Housing/data/raw/ASMTGIS_Table.csv")

landvalue_df <- bfs_df %>%
  group_by(tmk) %>%
  # if row has suffix == "0000", keep only that; otherwise keep the first row
  filter(if (any(suffix == "0000")) suffix == "0000" else row_number() == 1) %>%
  ungroup() %>%
  # keep only tmk and landvalue
  select(tmk, landvalue)

# it thinks tmk is an integer
landvalue_df <- landvalue_df %>%
  mutate(tmk = as.character(tmk))

# join landvalue & parcels
parcels <- parcels %>%
  left_join(landvalue_df, by = "tmk")



