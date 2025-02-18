## ---------------------------
##
## Script name: table_s3
##
## Purpose of script:
##    Create table s3, proportion of tribal lands classified as DACs by tools
##
## Author: Claire Morton
##
## Date Created: 2023-05-03
##
## Email: mortonc@stanford.edu
##
## ---------------------------
##
## Notes:
##
## ---------------------------

# Install packages ---------------------------
library(readr)
library(tidyverse)
library(dataframe)
library(ggpubr)
library(tigris)
library(tidycensus)
library(sf)
library(stargazer) # for table exporting
sf::sf_use_s2(FALSE)

# Load data ---------------------------
ca_state <- states(year = 2018) %>%
  filter(NAME == "California") 

nad83 <- "EPSG:4269"
proj <- "EPSG: 32610"
state <- st_crs(ca_state)

ca_state <- ca_state %>%
  st_transform(proj)

tribal_lands <- read_csv("data/data_raw/CEJST_data/BIA_National_LAR.csv")
tribal_lands <- st_as_sf(tribal_lands, wkt = "polygon")
tribal_lands <- tribal_lands %>%
  st_set_crs(nad83) %>%
  st_transform(proj) %>%
  st_make_valid()

data <- read_csv("data/data_processed/metrics_block_group.csv") %>% # Load block group DAC data
  select(GEOID, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)

#tigris_shp <- native_areas(year = 2018)

# tigris_shp_intersect <- tigris_shp[st_intersects(tigris_shp, ca_state)%>% lengths > 0,]
# tigris_shp_within <- tigris_shp[st_within(tigris_shp, ca_state)%>% lengths > 0,]
# 
# setdiff(tigris_shp_intersect$NAME, tigris_shp_within$NAME)
# 
# 
# tribal_lands_intersect <- tribal_lands[st_intersects(tribal_lands, ca_state)%>% lengths > 0,]
# tribal_lands_within <- tribal_lands[st_within(tribal_lands, ca_state)%>% lengths > 0,]

# Intersect tribal lands with state boundary ------------------
tribal_lands_intersect <- tribal_lands[st_intersects(tribal_lands, ca_state)%>% lengths > 0,]
tribal_lands_intersection <- st_intersection(tribal_lands_intersect, ca_state)

# Match block groups to tribal lands intersected with state boundary  ------------------
block_group_sf <- tigris::block_groups(state = "CA", year = 2019) %>%
  select(GEOID, ALAND) %>%
  st_transform(proj)

block_group_intersection <- st_intersection(block_group_sf, tribal_lands_intersection)  %>%
  select(GEOID) %>%
  st_transform(nad83)
block_group_intersection$area <- st_area(block_group_intersection) # find area in square meters

block_group_intersection <- block_group_intersection %>% # get area covered by each block group
  group_by(GEOID) %>%
  st_drop_geometry()%>%
  mutate(area = sum(area)) %>%
  unique()

merged <- merge(data, block_group_intersection)
total_tribal_land_area <- sum(st_area(tribal_lands_intersection))

# Get proportion of tribal lands covered by the tools ------------------
cejst <- 1 # CEJST is defined to cover all of these areas
ces <- as.double(sum(merged[merged$ces_dac==1,]$area, na.rm = T)/total_tribal_land_area)
ces_adj <- as.double(sum(merged[merged$ces_dac_adj==1,]$area, na.rm = T)/total_tribal_land_area)
eji <- as.double(sum(merged[merged$eji_dac==1,]$area, na.rm = T)/total_tribal_land_area)
trivariate <- as.double(sum(merged[merged$univariate_dac==1,]$area, na.rm = T)/total_tribal_land_area)

vals <- c(ces, ces_adj, trivariate, eji, cejst)

# Normalize by land area of California covered by tools ------------------
block_group <- block_groups(
  state = "CA",
  year = 2019) %>%
  dplyr::select(ALAND, GEOID, geometry) %>%
  mutate(ALAND = 3.86102e-7 * ALAND) # convert from square meters to square miles
total_land_area <- sum(block_group$ALAND, na.rm = T)
# Merge data
merged <- merge(data, block_group, by = "GEOID")

# Land area
land_area <- merged %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac, ALAND) %>%
  mutate(across(c(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac), ~ . * ALAND)) %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac) %>%
  colSums(na.rm = T)

# Get CEJST land area of DACs (not simply adding block group areas, since CEJST designates all tribal lands)
cejst_land_area <- merged %>%
  st_as_sf() %>%
  st_transform(proj) %>%
  filter(cejst_dac == 1) %>%
  st_union(tribal_lands_intersection)

# Calculate normalized proportions of land area classified as DACs (dividing by proportion of CA classified as DACs)
cejst_norm <- as.double(cejst/(land_area["cejst_dac"]/total_land_area))
ces_norm <- as.double(ces/(land_area["ces_dac"]/total_land_area))
ces_adj_norm <- as.double(ces_adj/(land_area["ces_dac_adj"]/total_land_area))
eji_norm <- as.double(eji/(land_area["eji_dac"]/total_land_area))
trivariate_norm <- as.double(trivariate/(land_area["univariate_dac"]/total_land_area))

norm_vals <- c(ces_norm, ces_adj_norm, trivariate_norm, eji_norm, cejst_norm)

# Join into table ---------------------------
row_names <- c("CES", "CES+", "Trivariate", "EJI", "CEJST")
table <- data.frame(row_names, paste0(round(vals, 2), " (", round(norm_vals, 2), ")"))
colnames(table) = c("Metric", "Proportion Tribal Lands in DACs (Normalized)")

stargazer(table, 
          summary=FALSE, rownames=FALSE,
          type = "html", out = "outputs/tables/table_s3.html")
