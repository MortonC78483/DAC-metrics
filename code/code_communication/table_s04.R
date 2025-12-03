## ---------------------------
##
## Script name: table_s4
##
## Purpose of script:
##    Create table s4, proportion of tribal lands classified as DACs by tools
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
library(lwgeom)
sf::sf_use_s2(FALSE)

# Load data ---------------------------
ca_state <- states(year = 2018) %>%
  filter(NAME == "California") 

nad83 <- "EPSG:4269"
proj <- "EPSG:32610"
state <- st_crs(ca_state)

ca_state <- ca_state %>%
  st_transform(proj)

tribal_lands <- read_csv("data/data_raw/CEJST_data/BIA_National_LAR.csv") %>%
  st_as_sf(wkt = "polygon") %>%
  st_set_crs("EPSG:4269") %>%
  st_transform(proj) %>%
  st_make_valid() %>%
  select(-description)
# clip to CA
tribal_lands <- tribal_lands[st_intersects(tribal_lands, ca_state)%>% lengths > 0,]

# fix any overlapping lands
tribal_lands <- st_union(tribal_lands) %>%
  st_make_valid()

data <- read_csv("data/data_processed/metrics_tract.csv") %>% # Load tract DAC data
  dplyr::select(GEOID, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)

# Match tracts to tribal lands intersected with state boundary  ------------------
tract_sf <- tigris::tracts(state = "CA", year = 2019) %>%
  dplyr::select(GEOID) %>%
  st_transform(proj) %>%
  mutate(tract_area = st_area(geometry))
ca_area = sum(tract_sf$tract_area)

# get intersection area between tribal lands and tracts, per tract -- 
tract_tribal <- st_intersection(tract_sf, tribal_lands) %>%
  mutate(tribal_area = st_area(geometry)) %>%   # area in square meters
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(tribal_area = sum(tribal_area))

# merge tract areas, DAC classifications, tract area
merged <- left_join(data, tract_tribal, by = "GEOID") %>%
  left_join(tract_sf %>% st_drop_geometry(), by = "GEOID") %>%
  mutate(tribal_area = replace_na(tribal_area, units::set_units(0, m^2)))

# get proportion of tribal lands designated as DACs
total_tribal_land <- merged %>%
  summarise(total = sum(tribal_area)) %>%
  pull(total)
eji_prop <- merged %>%
  filter(eji_dac == 1) %>%
  summarise(area = sum(tribal_area)) %>%
  mutate(prop = area / total_tribal_land) %>%
  pull(prop)
trivariate_prop <- merged %>%
  filter(univariate_dac == 1) %>%
  summarise(area = sum(tribal_area)) %>%
  mutate(prop = area / total_tribal_land) %>%
  pull(prop)
ces_prop <- 1
ces_adj_prop <- 1
cejst_prop <- 1

# get land area covered by all tools
total_eji_area <- merged %>%
  filter(eji_dac == 1) %>%
  summarise(area = sum(tract_area)) %>%
  pull(area)

total_trivariate_area <- merged %>%
  filter(univariate_dac == 1) %>%
  summarise(area = sum(tract_area)) %>%
  pull(area)

ces_dac_aland <- merged %>%
  filter(ces_dac == 1) %>%
  summarise(area = sum(tract_area)) %>%
  pull(area)
ces_tribal_non_dac <- merged %>%
  filter(ces_dac == 0) %>%
  summarise(area = sum(tribal_area)) %>%
  pull(area)
total_ces_area <- ces_dac_aland + ces_tribal_non_dac

ces_adj_aland <- merged %>%
  filter(ces_dac_adj == 1) %>%
  summarise(area = sum(tract_area)) %>%
  pull(area)
ces_adj_tribal_non_dac <- merged %>%
  filter(ces_dac_adj == 0) %>%
  summarise(area = sum(tribal_area)) %>%
  pull(area)
total_ces_adj_area <- ces_adj_aland + ces_adj_tribal_non_dac

cejst_dac_aland <- merged %>%
  filter(cejst_dac == 1) %>%
  summarise(area = sum(tract_area)) %>%
  pull(area)
cejst_tribal_non_dac <- merged %>%
  filter(cejst_dac == 0) %>%
  summarise(area = sum(tribal_area)) %>%
  pull(area)
total_cejst_area <- cejst_dac_aland + cejst_tribal_non_dac

land_areas = list(
  EJI_area = eji_prop/(total_eji_area/ca_area),
  Trivariate_area = trivariate_prop/(total_trivariate_area/ca_area),
  CES_area = ces_prop/(total_ces_area/ca_area),
  CES_adj_area = ces_adj_prop/(total_ces_adj_area/ca_area),
  CEJST_area = cejst_prop/(total_cejst_area/ca_area))



# Vectorize ---------------------------
tribal_land_proportions_vec <- c(ces_prop, 
                                 ces_adj_prop,
                                 trivariate_prop,
                                 eji_prop,
                                 cejst_prop)

land_areas_vec <- c(ces_prop/(total_ces_area/ca_area),
                    ces_adj_prop/(total_ces_adj_area/ca_area),
                    trivariate_prop/(total_trivariate_area/ca_area),
                    eji_prop/(total_eji_area/ca_area),
                    cejst_prop/(total_cejst_area/ca_area))


# Join into table ---------------------------
row_names <- c("CES", "CES+", "Trivariate", "EJI", "CEJST")
table <- data.frame(row_names, paste0(round(tribal_land_proportions_vec, 2), 
                                      " (", round(land_areas_vec, 2), ")"))
colnames(table) = c("Metric", "Proportion Tribal Lands in DACs (Normalized)")

stargazer(table,
          summary=FALSE, rownames=FALSE,
          type = "html", out = "outputs/tables/table_s4.html")
