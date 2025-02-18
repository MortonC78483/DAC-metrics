## ---------------------------
##
## Script name: 06_analyze_dasymetric_intersection
##
## Purpose of script:
##    determine the proportion of land designated as a DAC greater than 1km from a populated area
##
## Author: Claire Morton
##
## Date Created: 2023-02-21
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
library(readr)
library(tigris)
library(tidyverse)
library(ggplot2) 
library(sf)
library(ggpubr)

# Load data ---------------------------
buffer <- read.csv("data/data_interim/dasymetric_intersection_1km.csv") %>%
  mutate(GEOID = paste0("0", as.character(GEOID)))
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(GEOID, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)
buffer_qgis <- read.csv("data/data_interim/1km_buildings_buffered_qgis.csv")  %>%
  mutate(GEOID = paste0("0", as.character(GEOID)))

# Merge buffer data with DAC data ---------------------------
merged <- merge(data, buffer, by = "GEOID", all = T) %>%
  dplyr::select(-c(X)) %>%
  mutate(prop_1km_populated = ifelse(is.na(prop_1km_populated), 0, prop_1km_populated)) # omitted block groups have no people
merged_qgis <- merge(data, buffer_qgis, by = "GEOID", all = T) %>%
  mutate(bufferprop = ifelse(is.na(bufferprop), 0, bufferprop)) # omitted block groups have no people

# Get proportion of land intersecting with each metric ---------------------------
# x % of land designated as DACs by these metrics is within 1km of a populated area
intersect_ces = sum(merged[merged$ces_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$ces_dac == 1,]$area_bg, na.rm = T)
intersect_ces_adj = sum(merged[merged$ces_dac_adj == 1,]$area_intersect, na.rm = T)/sum(merged[merged$ces_dac_adj == 1,]$area_bg, na.rm = T)
intersect_univariate = sum(merged[merged$univariate_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$univariate_dac == 1,]$area_bg, na.rm = T)
intersect_eji = sum(merged[merged$eji_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$eji_dac == 1,]$area_bg, na.rm = T)
intersect_cejst = sum(merged[merged$cejst_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$cejst_dac == 1,]$area_bg, na.rm = T)

# Find how mnay block groups had no intersection within 1km of buffer, and make sure this makes sense ---------------------------
# How many have no population? 72, and out of these, only 3 don't actually intersect with any building buffers
no_pop <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  filter(total_race_eth == 0)

summary(setdiff(data$GEOID, buffer$GEOID) %in% no_pop$GEOID) # get how many of the BG with no building intersections have no population
summary(buffer_qgis[is.na(buffer_qgis$bufferprop),]$GEOID %in% no_pop$GEOID) # get how many of the BG with no building intersections have no population (from QGIS)


# intersect_ces_qgis = sum(merged_qgis[merged_qgis$ces_dac == 1,]$buffer_Area, na.rm = T)/sum(merged_qgis[merged_qgis$ces_dac == 1,]$area, na.rm = T)
# intersect_ces_adj_qgis = sum(merged_qgis[merged_qgis$ces_dac_adj == 1,]$buffer_Area, na.rm = T)/sum(merged_qgis[merged_qgis$ces_dac_adj == 1,]$area, na.rm = T)
# intersect_univariate_qgis = sum(merged_qgis[merged_qgis$univariate_dac == 1,]$buffer_Area, na.rm = T)/sum(merged_qgis[merged_qgis$univariate_dac == 1,]$area, na.rm = T)
# intersect_eji_qgis = sum(merged_qgis[merged_qgis$eji_dac == 1,]$buffer_Area, na.rm = T)/sum(merged_qgis[merged_qgis$eji_dac == 1,]$area, na.rm = T)
# intersect_cejst_qgis = sum(merged_qgis[merged_qgis$cejst_dac == 1,]$buffer_Area, na.rm = T)/sum(merged_qgis[merged_qgis$cejst_dac == 1,]$area, na.rm = T)

# key may be that we don't care about water block groups -- filter out block groups with no land area ---------------------------
# nonzero_land_area <- tigris::block_groups(state = "CA", year = 2019) %>%
#   select(GEOID, ALAND) %>%
#   filter(ALAND != 0)
# nonzero_land_area <- nonzero_land_area$GEOID
# merged <- merged %>%
#   filter(GEOID %in% nonzero_land_area)
# # x % of land designated as DACs by these metrics is not within 1km of a populated area
# intersect_ces2 = sum(merged[merged$ces_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$ces_dac == 1,]$area_bg, na.rm = T)
# intersect_ces_adj2 = sum(merged[merged$ces_dac_adj == 1,]$area_intersect, na.rm = T)/sum(merged[merged$ces_dac_adj == 1,]$area_bg, na.rm = T)
# intersect_univariate2 = sum(merged[merged$univariate_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$univariate_dac == 1,]$area_bg, na.rm = T)
# intersect_eji2 = sum(merged[merged$eji_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$eji_dac == 1,]$area_bg, na.rm = T)
# intersect_cejst2 = sum(merged[merged$cejst_dac == 1,]$area_intersect, na.rm = T)/sum(merged[merged$cejst_dac == 1,]$area_bg, na.rm = T)


