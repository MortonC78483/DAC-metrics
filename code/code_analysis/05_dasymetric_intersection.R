## ---------------------------
##
## Script name: 05_dasymetric_intersection
##
## Purpose of script:
##    Intersect buffered dasymetric data with metrics
##
## Author: Claire Morton
##
## Date Created: 2023-02-15
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

# Set up CRS ---------------------------
# unprojected CRS, NAD83, for geographic data
crs_nad83 <- st_crs("+init=epsg:4269 +proj=longlat +ellps=GRS80
                        +datum=NAD83 +no_defs +towgs84=0,0,0")  

# Load data ---------------------------
polygons_buffered <- read_sf("data/data_interim/populated_areas_1km_buffer.shp") %>%
  st_set_crs(crs_nad83)
polygons_valid <- st_make_valid(polygons_buffered)

# load tigris shapefiles
tract_sf <- tigris::tracts(state = "CA", year = 2019) %>%
  select(GEOID, ALAND) %>%
  st_set_crs(crs_nad83)
st_write(tract_sf, "data/data_raw/tracts_2019.shp", 
         append = FALSE)

# Intersect block groups with dasymetric data ---------------------------
# get proportion of each block group that is > buffer distance from populated areas
intersect <- st_intersection(polygons_valid, tract_sf)

#write.csv(intersect %>% st_drop_geometry(), "intersection_file.csv")
tract_sf <- tract_sf %>%
  mutate(area_tract = st_area(.) %>% as.numeric())

areas <- intersect %>% 
  mutate(area_intersect = st_area(.) %>% as.numeric()) %>%
  st_drop_geometry()
areas <- merge(areas, tract_sf) %>%
  mutate(prop_1km_populated = area_intersect/area_tract) %>%
  select(GEOID, area_tract, area_intersect, prop_1km_populated)

write.csv(areas, "data/data_interim/dasymetric_intersection_tract_1km.csv")


