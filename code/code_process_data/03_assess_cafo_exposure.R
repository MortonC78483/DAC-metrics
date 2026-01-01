## ---------------------------
##
## Script name: 03_assess_cafo_exposure
##
## Purpose of script:
##    Load CAFO locations, create dataset containing site weights
##    for CAFOs based on size and proximity to nearest populated census block. Weight
##    is 1 for 0-250m, .5 for 250-500m, .25 for 500-750m, and .1 for 750-1000m.
##    Sum weights within tracts, export dataset of tract GEOID, weights, and
##    weight percentile.
##
## Author: Claire Morton
##
## Date Created: 2022-11-21
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
# Used to sum CAFO exposure to populated block groups by census tract
scale_distances <- function(distances, weights) {
  sum = 0
  for (i in 1:length(distances)) {
    if (distances[i] <= 250) {
      sum = sum + 1*weights[i]
    } else if (distances[i] <= 500) {
      sum = sum + .5*weights[i]
    } else if (distances[i] <= 750) {
      sum = sum + .25*weights[i]
    } else if (distances[i] <= 1000) {
      sum = sum + .1*weights[i]
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

# Load CAFOs, blocks, and tracts data ---------------------------
block <- read_sf("data/data_interim/census_block_populated/CA_census_block_populated.shp")
block_proj <- st_transform(block, crs_projected)
cafos <- read_sf("data/data_raw/cafo_facilities.csv")
tract <- tracts(
  state = "CA",
  year = 2019
) %>%
  select(STATEFP, COUNTYFP, TRACTCE, GEOID, geometry)

cafos <- cafos %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  select(latitude, longitude, facility_id, footprint_sq_m)
cafos_sf <- sf::st_as_sf(cafos, coords = c("longitude", "latitude"), crs = st_crs(tract))

# Calculate scaled CAFO counts per tract---------------------------
# create 1000 m buffer around census tracts
tract_proj_buffered <- st_transform(tract, crs_projected) %>%
  st_buffer(1000)

# create dataset of CAFOs in every buffered tract, where geometry is well locations
cafos_proj <- st_transform(cafos_sf, crs_projected)
cafos_in_tract <- st_join(cafos_proj, tract_proj_buffered, left = FALSE)

# for each tract, obtain populated census blocks and calculate the distance from CAFO
# to closest populated census block (in meters), scale oil wells, and append to
# new dataframe
tract_cafos_scaled <- data.frame("GEOID" = character(), "scaled" = numeric())

for (tract_id in tract$GEOID) {
  # find relevant cafos and blocks
  cafos_in_tract_filtered <- cafos_in_tract %>%
    filter(GEOID == tract_id)
  block_filtered <- block_proj %>%
    filter(GEOID == tract_id)
  # if we have any cafos/populated blocks, scale cafo distances to populated blocks
  if (nrow(cafos_in_tract_filtered) == 0 || nrow(block_filtered) == 0) {
    scaled_value <- 0
  } else {
    nearest <- st_nearest_feature(cafos_in_tract_filtered, block_filtered)
    distances <- st_distance(cafos_in_tract_filtered, block_filtered[nearest,], by_element=TRUE)
    weights <- rep(1, length(distances))#cafos_in_tract_filtered$Weight
    scaled_value <- scale_distances(as.vector(distances), as.vector(weights))
  }
  # add tract id and scaled cafo distance to dataframe
  to_append <- data.frame("GEOID" = tract_id, "scaled" = scaled_value)
  tract_cafos_scaled <- rbind(tract_cafos_scaled, to_append)
}

# Append percentiles to dataframe  ---------------------------
tract_cafos_scaled_nonzero <- tract_cafos_scaled %>%
  filter(scaled != 0) %>%
  mutate(percentile = percent_rank(scaled))

tract_cafos_scaled_zero <- tract_cafos_scaled %>%
  filter(scaled == 0) %>%
  mutate(percentile = 0)

tract_cafos_scaled_percentiles <- rbind(tract_cafos_scaled_nonzero, tract_cafos_scaled_zero)
colnames(tract_cafos_scaled_percentiles) <- c("GEOID", "scaled_cafo_score", "percentile_cafo_score")

# Export data  ---------------------------
write.csv(tract_cafos_scaled_percentiles, "data/data_interim/census_tract_cafos.csv")

