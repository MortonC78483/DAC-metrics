## ---------------------------
##
## Script name: figure_01
##
## Purpose of script:
##    Create figure 1, mapping DACs on all five metrics 
##    and map of combined metric designations
##
## Author: Claire Morton
##
## Date Created: 2022-12-23
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

# Set up colors ---------------------------
color_vector_2 <- c('white', '#bd0026')
color_vector_6 <- c('#ffffff', '#ffffb2', '#fecc5c', '#fd8d3c', '#f03b20', '#bd0026')

# Import data ---------------------------
# ces <- read.csv("data/data_interim/recalculated_ces_scores.csv") %>%
#   mutate(GEOID = paste0("0", GEOID))
metrics_block_group <- read.csv("data/data_processed/metrics_block_group.csv")%>%
  mutate(GEOID = paste0("0", GEOID),
         tract = paste0("0", tract)) %>%
  dplyr::select(GEOID, tract, ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac, n_dac)
metrics_tract <- metrics_block_group %>%
  dplyr::select(-c(GEOID, n_dac, univariate_dac)) %>%
  unique()

tract <- tracts(
  state = "CA",
  year = 2019) %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, geometry)
block_group <- block_groups(
  state = "CA",
  year = 2019) %>%
  dplyr::select(STATEFP, COUNTYFP, BLKGRPCE, GEOID, geometry)

metrics_block_group_sf <- merge(block_group, metrics_block_group, by = "GEOID") %>%
  st_as_sf()
metrics_tract_sf <- merge(tract, metrics_tract, by.x = "GEOID", by.y = "tract")

# Simplify data ---------------------------
metrics_block_group_s_sf <- st_simplify(metrics_block_group_sf, 10, dTolerance = 50)
metrics_tract_s_sf <- st_simplify(metrics_tract_sf, 10, dTolerance = 50)

# Make all data polygons ------------------e
metrics_block_group_s_sf$geometry <- sf::st_cast(metrics_block_group_s_sf$geometry, "MULTIPOLYGON")
metrics_block_group_s_sf$geometry <- sf::st_cast(metrics_block_group_s_sf$geometry, "POLYGON", group_or_split = FALSE)

metrics_tract_s_sf$geometry <- sf::st_cast(metrics_tract_s_sf$geometry, "MULTIPOLYGON")
metrics_tract_s_sf$geometry <- sf::st_cast(metrics_tract_s_sf$geometry, "POLYGON", group_or_split = FALSE)

# Create part a ---------------------------
part_a <- ggplot(data = metrics_tract_s_sf, 
       aes(fill = as.factor(ces_dac))) +
  geom_sf(size = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("CES") +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "grey"))      

# Create part b ---------------------------
part_b <- ggplot(data = metrics_tract_s_sf, 
                 aes(fill = as.factor(ces_dac_adj))) +
  geom_sf(size = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("CES+") +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "grey"))           

# Create part c ---------------------------
part_c <- ggplot(data = metrics_tract_s_sf, 
                 aes(fill = as.factor(cejst_dac))) +
  geom_sf(size = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("CEJST") +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "grey")) 

# Create part d ---------------------------
part_d <- ggplot(data = metrics_tract_s_sf, 
                 aes(fill = as.factor(eji_dac))) +
  geom_sf(size = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("EJI") +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "grey"))

# Create part e ---------------------------
part_e <- ggplot(data = metrics_block_group_s_sf, 
                 aes(fill = as.factor(univariate_dac))) +
  geom_sf(size = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("Trivariate") +
  theme_void() +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "grey"))

# Create part f ---------------------------
part_f <- ggplot(data = metrics_block_group_s_sf, 
       aes(fill = as.factor(n_dac))) +
  geom_sf(size = 0) +
  scale_fill_manual(values = color_vector_6, na.value="white",
                    name = "Number of Metrics\nClassifying as\nDisadvantaged") +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey"))

# Arrange figure parts and export ---------------------------
figure_1 <- cowplot::plot_grid(part_a, part_b, part_c, part_d, part_e, part_f, 
           nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")
cowplot::ggsave2("outputs/figures/fig_1.png", figure_1,
                 width = 13,
                 height = 9,
                 units = c("in"))
