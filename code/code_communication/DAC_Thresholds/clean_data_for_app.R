## ---------------------------
##
## Script name: clean_data_for_app
##
## Purpose of script:
##    Set up simplified shapefile of block groups and univariate threshold information to be mapped
##    in a Shiny web app.
##
## Author: Claire Morton
##
## Date Created: 2022-11-03
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
library(sf)
library(tidycensus)
library(tigris)
library(rgeos)

# Load data ---------------------------
block_group_data <- read_csv("data/data_interim/block_group_stability.csv") %>%
  select(GEOID, propHispanicOrNonWhite, propPoverty, propLEP)

block_group <- block_groups(
  state = "CA",
  year = 2019) %>%
  select(STATEFP, COUNTYFP, BLKGRPCE, GEOID, geometry)

block_group_data_sf <- merge(block_group_data, block_group, by = "GEOID") %>%
  select(propHispanicOrNonWhite, propPoverty, propLEP, geometry) %>%
  st_as_sf()

# Simplify data ---------------------------
simpl<- st_simplify(block_group_data_sf, 10, dTolerance = 50)

# Make all data polygons ------------------e
simpl$geometry <- sf::st_cast(simpl$geometry, "MULTIPOLYGON")
simpl$geometry <- sf::st_cast(simpl$geometry, "POLYGON", group_or_split = FALSE)

# Export data ---------------------------
st_write(simpl, "code/code_communication/DAC_Thresholds/block_group_data_sf.shp")

