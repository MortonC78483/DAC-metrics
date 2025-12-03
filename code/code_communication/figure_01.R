## ---------------------------
##
## Script name: figure_01
##
## Purpose of script:
##    Create figure mapping DACs on all five metrics 
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
color_vector_2 <- c('white', '#bf7060')
color_vector_6 <- c('#ffffff', '#ffffb2', '#fecc5c', '#fd8d3c', '#f03b20', '#bd0026')

# Import data ---------------------------
# ces <- read.csv("data/data_interim/recalculated_ces_scores.csv") %>%
#   mutate(GEOID = paste0("0", GEOID))
metrics_tract <- read.csv("data/data_processed/metrics_tract.csv")%>%
  mutate(GEOID = paste0("0", GEOID)) %>%
  dplyr::select(GEOID, ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac, n_dac)

tract <- tracts(
  state = "CA",
  year = 2019) %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, geometry)
states <- states(cb = TRUE) %>%
  filter(NAME == "California")

metrics_tract_sf <- merge(tract, metrics_tract, by = "GEOID")

# Simplify data ---------------------------
metrics_tract_s_sf <- st_simplify(metrics_tract_sf, 10, dTolerance = 50)

# Make all data polygons ------------------e
metrics_tract_s_sf$geometry <- sf::st_cast(metrics_tract_s_sf$geometry, "MULTIPOLYGON")
metrics_tract_s_sf$geometry <- sf::st_cast(metrics_tract_s_sf$geometry, "POLYGON", group_or_split = FALSE)

# Create part a ---------------------------
part_a <- ggplot() +
  geom_sf(data = metrics_tract_s_sf, 
          aes(fill = as.factor(ces_dac)), linewidth = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("CES") +
  theme_void() +
  theme(legend.position = "none") +
  geom_sf(data = states, linewidth = .5, fill = "transparent")
  #theme(panel.background = element_rect(fill = "grey"))      

# Create part b ---------------------------
part_b <- ggplot() +
  geom_sf(data = metrics_tract_s_sf, 
          aes(fill = as.factor(ces_dac_adj)), linewidth = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("CES+") +
  theme_void() +
  theme(legend.position = "none") +
  geom_sf(data = states, linewidth = .5, fill = "transparent")# +
  #theme(panel.background = element_rect(fill = "grey"))           

# Create part c ---------------------------
part_c <- ggplot() +
  geom_sf(data = metrics_tract_s_sf, 
          aes(fill = as.factor(cejst_dac)), linewidth = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("CEJST") +
  theme_void() +
  theme(legend.position = "none") +
  geom_sf(data = states, linewidth = .5, fill = "transparent")# +
  #theme(panel.background = element_rect(fill = "grey")) 

# Create part d ---------------------------
part_d <- ggplot() +
  geom_sf(data = metrics_tract_s_sf, 
          aes(fill = as.factor(eji_dac)), linewidth = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("EJI") +
  theme_void() +
  theme(legend.position = "none") +
  geom_sf(data = states, linewidth = .5, fill = "transparent")# +
  #theme(panel.background = element_rect(fill = "grey"))

# Create part e ---------------------------
part_e <- ggplot() +
  geom_sf(data = metrics_tract_s_sf, 
          aes(fill = as.factor(univariate_dac)), linewidth = 0) +
  scale_fill_manual(values = color_vector_2, na.value="white") +
  ggtitle("Trivariate") +
  theme_void() +
  theme(legend.position = "none") +
  geom_sf(data = states, linewidth = .5, fill = "transparent")# +
  #theme(panel.background = element_rect(fill = "grey"))

# Create part f ---------------------------
# count how many tracts per dac type
n_bg_per_type = metrics_tract_s_sf %>%
  st_drop_geometry() %>%
  group_by(n_dac) %>%
  summarize(sum = paste0("(",n(),")"))

legend_names = paste(seq(0:5), n_bg_per_type$sum)

part_f <- ggplot() +
  geom_sf(data = metrics_tract_s_sf, 
          aes(fill = as.factor(n_dac)), linewidth = 0) +
  scale_fill_manual(labels = legend_names,
                    values = color_vector_6, na.value="white",
                    name = "Number of Tools\nClassifying as\nDisadvantaged") +
  theme_void() +
  geom_sf(data = states, linewidth = .5, fill = "transparent")# +
  #theme(panel.background = element_rect(fill = "grey"))

# Arrange figure parts and export ---------------------------
figure_1 <- cowplot::plot_grid(part_a, part_b, part_c, part_d, part_e, part_f, 
           nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")

cowplot::ggsave2("outputs/figures/fig_01.png", figure_1,
                 width = 13,
                 height = 9,
                 units = c("in"))
