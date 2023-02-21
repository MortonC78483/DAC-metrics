## ---------------------------
##
## Script name: 08_buffer_dasymetric_data
##
## Purpose of script:
##    Create 1km (then 3km, 5km) buffers around 
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

# Load data ---------------------------
polygons <- read_sf("data/data_raw/Nick_CA_PopulationMap/SHP/ca_popareas_polys.shp")

# Set up CRS ---------------------------
# unprojected CRS, NAD83, for geographic data
crs_nad83 <- st_crs("+init=epsg:4269 +proj=longlat +ellps=GRS80
                        +datum=NAD83 +no_defs +towgs84=0,0,0")  

# projected CRS, for creating buffers
crs_projected <- st_crs("+proj=utm +zone=11 +datum=WGS84") 

# project and merge 1km buffer ---------------------------
polygons_proj <- st_transform(polygons, crs_projected)
polygons_1km <- st_buffer(polygons_proj, 1000)

polygons_1km_merged <- st_union(polygons_1km)

polygons_1km_merged_reprojected <- st_transform(polygons_1km_merged, crs_nad83)

# write data ---------------------------
st_write(polygons_1km_merged_reprojected, "data/data_interim/populated_areas_1km_buffer.shp")



