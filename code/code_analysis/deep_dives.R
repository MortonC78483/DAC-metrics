# deep dives on specific sites for community partner

# libraries
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)


# Figure out which census block group a place is in
find_GEOID_intersect  <- function (bg, dac_data, coord, name){
  point <- st_point(x = coord)
  geoid <- bg[st_intersects(point, bg)[[1]],]$GEOID
  dac_des <- dac_data[dac_data$GEOID==geoid,]
  return(paste0(name, " is in DACs according to: ", ifelse(dac_des$univariate_dac == 1, "Trivariate ", ""),
         ifelse(dac_des$ces_dac == 1, "CES ", ""),
         ifelse(dac_des$ces_dac_adj == 1, "CES+ ", ""),
         ifelse(dac_des$eji_dac == 1, "EJI ", ""),
         ifelse(dac_des$cejst_dac == 1, "CEJST ", "")))
}

# Load census block group data
# load tigris shapefiles
block_group_sf <- tigris::block_groups(state = "CA", year = 2019) %>%
  select(GEOID, ALAND) %>%
  st_transform("EPSG:4326")

# load DAC designations
dac_data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  select(GEOID, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)

# Majestic Warehouse (Greenfield) (35.285374871243235, -119.02405301076836)
find_GEOID_intersect(block_group_sf, dac_data, c(-119.02405301076836, 35.285374871243235), "Majestic warehouse")

# North Pointe Business Park (36.68731175233475, -119.77030163330969)
find_GEOID_intersect(block_group_sf, dac_data, c(-119.77030163330969, 36.68731175233475), "North Pointe Business Park")

# Crystal Geyser (35.368280811213936, -118.98540705921515)
find_GEOID_intersect(block_group_sf, dac_data, c(-118.98540705921515, 35.368280811213936), "Crystal Geyser")

# Sierra Recycling and Demolition (35.35475505716069, -118.98374045894528)
find_GEOID_intersect(block_group_sf, dac_data, c(-118.98374045894528, 35.35475505716069), "Sierra Recycling and Demolition")

# Bakersfield wastewater treatment plant (35.32562147685024, -118.97324724378123)
find_GEOID_intersect(block_group_sf, dac_data, c(-118.97324724378123, 35.32562147685024), "Bakersfield wastewater treatment plant ")










#sf::st_is_within_distance(pnt_sf, kern, 250)
#block_group_sf[which(st_intersects(pnt_sf, kern)), ]$GEOID

# point_df <- data.frame(x = 35.2854143,y = -119.0289829)
# ggplot(block_group_sf[st_intersects(point, block_group_sf)[[1]],])+
#   geom_sf()+
#   geom_point(data = point_df, aes(x = y, y = x,color = "red"))

