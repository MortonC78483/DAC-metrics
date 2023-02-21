## ---------------------------
##
## Script name: figure_s1
##
## Purpose of script:
##    Create figure s1, distributions of population density, poverty, and 
##    Hispanic/Latino people proportion in DACs for all metrics
##
## Author: Claire Morton
##
## Date Created: 2022-01-12
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
library(tidyverse)
library(cowplot)

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(total_race_eth, pop_density, nonhispanic_black, over_200_percent_poverty, total_poverty, hispanic,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty,
         hispanic_prop = hispanic/total_race_eth,
         nh_black_prop = nonhispanic_black/total_race_eth,
         pop_density = pop_density * 1000000) %>% # people/km^2
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic, nonhispanic_black))

# Format data ---------------------------
# Create dataset with DAC designation type and variables of interest
univariate_dac_data <- data %>%
  dplyr::filter(univariate_dac == 1) %>%
  mutate(dac = "univariate")
ces_dac_data <- data %>%
  dplyr::filter(ces_dac == 1) %>%
  mutate(dac = "ces")
ces_dac_adj_data <- data %>%
  dplyr::filter(ces_dac_adj == 1) %>%
  mutate(dac = "ces_adj")
eji_dac_data <- data %>%
  dplyr::filter(eji_dac == 1) %>%
  mutate(dac = "eji")
cejst_dac_data <- data %>%
  dplyr::filter(cejst_dac == 1) %>%
  mutate(dac = "cejst")
total_data <- data %>%
  mutate(dac = "total")

dac_data <- rbind(univariate_dac_data, ces_dac_data, ces_dac_adj_data, eji_dac_data, cejst_dac_data, total_data) %>%
  dplyr::select(-c(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac))

# Create figure components ---------------------------
part_a <- ggplot(dac_data, aes(x = pop_density, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  ylab("") +
  xlab("Population Density")
part_b <- ggplot(dac_data, aes(x = poverty_prop, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  ylab("") +
  xlab("Proportion of Households in Poverty")
part_c <- ggplot(dac_data, aes(x = hispanic_prop, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  ylab("") +
  xlab("Proportion of Hispanic/Latino Residents")
part_d <- ggplot(dac_data, aes(x = nh_black_prop, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  ylab("") +
  xlab("Proportion of Non-Hispanic Black Residents")

# Arrange figure parts and export ---------------------------
figure_s1 <- cowplot::plot_grid(part_a, part_b, part_c, part_d,
                               nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")
cowplot::ggsave2("outputs/figures/fig_s1.png", figure_s1,
                 width = 13,
                 height = 9,
                 units = c("in"))

