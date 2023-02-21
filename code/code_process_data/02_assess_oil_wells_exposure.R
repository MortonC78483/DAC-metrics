## ---------------------------
##
## Script name: clean_census_data
##
## Purpose of script:
##    Load CalGEM oil well locations, create dataset containing site weights
##    for oil wells based on proximity to nearest populated census block. Weight
##    is 1 for 0-250m, .5 for 250-500m, .25 for 500-750m, and .1 for 750-1000m.
##    Sum weights within tracts, export dataset of tract GEOID, weights, and 
##    weight percentile.
##
## Author: Claire Morton
##
## Date Created: 2022-09-30
##
## Email: mortonc@stanford.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# Install packages ---------------------------
library(tidyverse)
library(tigris)
library(sf)

# function to scale a distance vector and return sum of scaled distances
# 1 for 0-250m, .5 for 250-500m, .25 for 500-750m, and .1 for 750-1000m
# Used to sum oil well exposure to populated block groups by census tract
scale_distances <- function(distances) {
  sum = 0
  for (i in 1:length(distances)) {
    if (distances[i] <= 250) {
      sum = sum + 1
    } else if (distances[i] <= 500) {
      sum = sum + .5
    } else if (distances[i] <= 750) {
      sum = sum + .25
    } else if (distances[i] <= 1000) {
      sum = sum + .1
    }
  }
  return (sum)
}

# Set up CRS ---------------------------
# unprojected CRS, NAD83, for geographic data
crs_nad83 <- st_crs("+init=epsg:4269 +proj=longlat +ellps=GRS80
                        +datum=NAD83 +no_defs +towgs84=0,0,0")  

# projected CRS, for creating buffers
crs_projected <- st_crs("+proj=utm +zone=11 +datum=WGS84") 

# Load oil wells, blocks, and tracts data ---------------------------
block <- read_sf("data/data_interim/census_block_populated/CA_census_block_populated.shp")
block_proj <- st_transform(block, crs_projected)
wells <- read_sf("data/wells_data/AllWells_gis/Wells_All.shp")
tract <- tracts(
  state = "CA",
  year = 2019
) %>%
  select(STATEFP, COUNTYFP, TRACTCE, GEOID, geometry)
  
wells_active <- wells %>%
  filter(WellStatus == "Active") %>%
  select(API, CountyName, Latitude, Longitude, geometry) %>%
  mutate(CountyCode = substr(API, 3, 5))

# Calculate scaled active oil well counts per tract---------------------------
# create 1000 m buffer around census tracts
tract_proj_buffered <- st_transform(tract, crs_projected) %>%
  st_buffer(1000)

# create dataset of wells in every buffered tract, where geometry is well locations
wells_active_proj <- st_transform(wells_active, crs_projected)
wells_in_tract <- st_join(wells_active_proj, tract_proj_buffered, left = FALSE)

# for each tract, obtain populated census blocks and calculate the distance from oil
# well to closest populated census block (in meters), scale oil wells, and append to 
# new dataframe
tract_wells_scaled <- data.frame("GEOID" = character(), "scaled" = numeric())

for (tract_id in tract$GEOID) {
  # find relevant wells and blocks
  wells_in_tract_filtered <- wells_in_tract %>%
    filter(GEOID == tract_id)
  block_filtered <- block_proj %>%
    filter(GEOID == tract_id)
  # if we have any wells/populated blocks, scale well distances to populated blocks
  if (nrow(wells_in_tract_filtered) == 0 || nrow(block_filtered) == 0) {
    scaled_value <- 0
  } else {
    nearest <- st_nearest_feature(wells_in_tract_filtered, block_filtered)
    distances <- st_distance(wells_in_tract_filtered, block_filtered[nearest,], by_element=TRUE)
    scaled_value <- scale_distances(as.vector(distances))
  }
  # add tract id and scaled well distance to dataframe
  to_append <- data.frame("GEOID" = tract_id, "scaled" = scaled_value)
  tract_wells_scaled <- rbind(tract_wells_scaled, to_append)
}

# Append percentiles to dataframe  ---------------------------
tract_wells_scaled_nonzero <- tract_wells_scaled %>%
  filter(scaled != 0) %>%
  mutate(percentile = percent_rank(scaled))

tract_wells_scaled_zero <- tract_wells_scaled %>%
  filter(scaled == 0) %>%
  mutate(percentile = 0)
  
tract_wells_scaled_percentiles <- rbind(tract_wells_scaled_nonzero, tract_wells_scaled_zero)
colnames(tract_wells_scaled_percentiles) <- c("GEOID", "scaled_well_count", "percentile_well_count")

# Export data  ---------------------------
write.csv(tract_wells_scaled_percentiles, "data/data_interim/census_tract_oil_wells.csv")

