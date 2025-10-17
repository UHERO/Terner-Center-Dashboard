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
library(stringr)

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

# intersect parcels with zones (creates multiple rows per parcel if it overlaps zones)
oahu_intersections <- st_intersection(oahutmk_filtered, zones_filtered)

# calculate overlap area 
oahu_intersections <- oahu_intersections %>%
  mutate(overlap_area = st_area(geometry))

# keep only the parcel with the largest overlap
oahu_with_zones <- oahu_intersections %>%
  group_by(tmk) %>%
  slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
  ungroup()

# convert rec_area_a (acres) to sqft and add to rec_area_s for lot sqft
oahu_parzon <- oahu_with_zones %>%
  mutate(
    lot_sqft = rec_area_s + ifelse(!is.na(rec_area_a), rec_area_a * 43560, 0)
  ) %>%
  select(
    tmk,
    lot_sqft,
    zone_class = ZONE_CLASS,
    geometry
  )

#=========================================================
# diagnostic map for the incorrect parcels
#=========================================================
library(tmap)
tmap_mode("view")

# a/amx parcels
parcels_A_AMX <- oahutmk_filtered %>%
  filter(tmk %in% oahu_parzon$tmk) %>%
  mutate(source = "A/AMX Filtered")

# parcels w/o a zone
parcels_missed <- oahutmk_filtered %>%
  filter(!tmk %in% oahu_parzon$tmk)

# map
tm_shape(zones_filtered) +
  tm_polygons(col = "lightblue", alpha = 0.2, border.col = "blue", lwd = 1) +
  tm_shape(parcels_A_AMX) +
  tm_borders(col = "green", lwd = 1.2) +
  tm_fill(col = "green", alpha = 0.3) +
  tm_shape(parcels_missed) +
  tm_borders(col = "red", lwd = 1.2) +
  tm_layout(
    title = "tmk & zone",
    legend.outside = TRUE
  )

#=========================================================
# Creates different columns for adopted & proposed TOD zones in parcels df with height
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

# create a df with the adopted data to filter out the A/AMX values
parcels_with_adopt <- st_join(oahu_parzon, tod_adopt, join = st_intersects)

target_zones <- c("A-1", "A-2", "A-3", "AMX-1", "AMX-2", "AMX-3")

parcels_with_adopt <- parcels_with_adopt %>%
  filter(to_zoning %in% target_zones)

# tmk shp files is weird, filter out the decimal columns 
filtered_parceled <- parcels_with_adopt[!grepl("\\.", rownames(parcels_with_adopt)), ]

# filter out the df
height_tod <- filtered_parceled %>%
  select(tmk, lot_sqft, to_zoning, to_map_hei) %>%
  rename(
    zone_class = to_zoning,
    height = to_map_hei
  )

# makes extracts height & makes it a numeric value 
height_tod <- height_tod %>%
  mutate(height = if_else(
    str_detect(height, "\\("),
    str_extract(height, "(?<=\\()\\d+"),  
    str_extract(height, "\\d+")           
  ),
  height = as.numeric(height)
  )

# create columns or rbind gets mad
height_tod <- height_tod %>%
  mutate(
    TOD_proposed = 0,
    TOD_adopt = 1
  ) 

#=========================================================
# Creates special districts column
#=========================================================

# loads in special districts 
special <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/Special_Districts_Zoning/Zoning_Special_District.shp")
# remove TOD special districts
special <- special %>%
  filter(special_di != "Transit-Oriented Development Special District")
# change CRS to match
special <- st_transform(special, st_crs(oahu_parzon)) 
# creates special districts column (0=no, 1=yes)
Special_district <- as.numeric(lengths(st_intersects(oahu_parzon, special)) > 0)

# create a df with the special districts
oahu_parzon <- st_join(oahu_parzon, special, join = st_intersects)

# create new df called parcels, drop geometry and remove tmk column
oahu_parzon <- oahu_parzon %>%
  mutate(TOD_proposed = TOD_proposed,
         TOD_adopt = TOD_adopt,
         Special_zone = Special_district,
         height = NA_real_) 

# joins parzon & TOD heights
parcels <- oahu_parzon %>%
  full_join(
    height_tod %>% st_drop_geometry(),  
    by = "tmk",
    suffix = c("_parzon", "_tod")
  ) %>%
  mutate(
    # if height exists in height_tod, use it
    height = coalesce(height_tod, height_parzon),
    zone_class = coalesce(zone_class_tod, zone_class_parzon),
    lot_sqft   = coalesce(lot_sqft_tod, lot_sqft_parzon),
    TOD_proposed = coalesce(TOD_proposed_parzon, TOD_proposed),
    TOD_adopt    = coalesce(TOD_adopt_parzon, TOD_adopt)
  ) %>%
  select(tmk, lot_sqft, zone_class, height, TOD_proposed, TOD_adopt, Special_zone, geometry)

#=========================================================
# Creates zoning height limits column
# uses geojson with manual fixes for missing attributes
#=========================================================

# load in height limit from geojson
height <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/height_zones.geojson")
height <- st_make_valid(height) 

# manually fill in missing height_label and height_label_source values
# these records have geometry but missing metadata
missing_data <- data.frame(
  objectid = c(23, 25, 28, 69, 89, 144, 145, 398, 399, 412, 440, 441, 442, 443, 445, 446, 448, 449, 450, 527, 529),
  height_label = c("40'", "60'", "220'", "150'", "250'", "40'", "40'", "350'", "60' (250')", "0'", 
                   "0'", "30'", "150'", "0'", "150'", "150'", "25'", "40'", "220'", "350'", "25'"),
  height_label_source = c("Zoning Map", "Zoning Map", "Waikiki Special District", "Zoning Map", "Zoning Map",
                          "Zoning Map", "Zoning Map", "Hawaii Capital Special District", 
                          "Transit-Oriented Development Special District", "Punchbowl Special District",
                          "Thomas Square Special District", "Punchbowl Special District", "Zoning Map",
                          "Punchbowl Special District", "Thomas Square Special District", "Thomas Square Special District",
                          "Thomas Square Special District", "Punchbowl Special District", 
                          "Diamond Head / Waikiki Special Districts", "Waikiki Special District", 
                          "Diamond Head Special District"),
  stringsAsFactors = FALSE
)

# fill in the missing values
for (i in 1:nrow(missing_data)) {
  oid <- missing_data$objectid[i]
  if (oid %in% height$objectid) {
    height$height_label[height$objectid == oid] <- missing_data$height_label[i]
    height$height_label_source[height$objectid == oid] <- missing_data$height_label_source[i]
  }
}

height_df <- height %>%
  select(height = height_label, source = height_label_source, geometry) %>%
  mutate(height_clean = if_else(
    str_detect(height, "\\("),
    str_extract(height, "(?<=\\()\\d+"),  
    str_extract(height, "\\d+")           
  ),
  height_clean = as.numeric(height_clean)
  ) %>%
  st_transform(st_crs(parcels)) 

# match parcels that intersect with height
parcels_with_height <- st_join(
  parcels,
  height_df %>% select(height_clean, source),
  join = st_intersects(),   
  left = TRUE
)

# add zoning height info only if TOD height is missing
# keep existing TOD height
parcels_with_height <- parcels_with_height %>%
  mutate(overlap_area = st_area(geometry)) %>%
  mutate(
    height = if_else(
      is.na(height),          
      height_clean,           
      height                  
    ),
    height_source = case_when(
      !is.na(height) & TOD_adopt == 1 ~ "TOD Zoning",
      is.na(height_clean) & TOD_adopt == 1 ~ "TOD Zoning",
      is.na(height) & !is.na(height_clean) ~ source,
      !is.na(height_clean) & TOD_adopt == 0 ~ source,
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    tmk,
    lot_sqft,
    zone_class,
    height,
    height_source,
    TOD_proposed,
    TOD_adopt,
    Special_zone,
    geometry
  )

# pick height zone with largest overlap
largest_overlap <- intersections %>%
  mutate(overlap_area = st_area(.)) %>%
  group_by(tmk) %>%
  slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(tmk, height_clean, source)

# rejoin height to original parcels
parcels_with_height <- parcels %>%
  left_join(st_drop_geometry(largest_overlap), by = "tmk") %>%
  mutate(
    height = if_else(is.na(height), height_clean, height),
    height_source = case_when(
      !is.na(height) & TOD_adopt == 1 ~ "TOD Zoning",
      is.na(height_clean) & TOD_adopt == 1 ~ "TOD Zoning",
      is.na(height) & !is.na(height_clean) ~ source,
      !is.na(height_clean) & TOD_adopt == 0 ~ source,
      TRUE ~ NA_character_
    )
  )

# # if height is NA and zone_class is A-1 or AMX-1 have the height column be 30 for parcels
# # A-2, A-3, AMX-2, AMX-3, is filled by the mode number (change?)
# parcels <- parcels %>%
#   mutate(
#     height = case_when(
#       is.na(height) & zone_class %in% c("A-1", "AMX-1") ~ 30,
#       is.na(height) & zone_class %in% c("A-2", "AMX-2") ~ 60,
#       is.na(height) & zone_class == "AMX-3" ~ 250,
#       is.na(height) & zone_class == "A-3" ~ 100,
#       TRUE ~ height
#     )
#   )
  
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
  filter(substr(parid, 9, 12) == "0000") %>%
  select(tmk, landvalue) %>%
  mutate(tmk = as.character(tmk))

# join landvalue & parcels
parcels <- parcels %>%
  left_join(landvalue_df, by = "tmk") %>%
  st_drop_geometry()

#=========================================================
# Adds in roads 
#
# NOTES: 
# use later for setback in buildable sqft 
# perhaps use to filter out private roads & parking lots
#=========================================================

# read in roads geojson
roads_df <- st_read("C:/Users/1saku/Desktop/Housing/data/raw/roads.geojson")