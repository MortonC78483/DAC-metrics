## ---------------------------
##
## Script name: calculate_numbers
##
## Purpose of script:
##    Calculate many of the numbers used in the paper (e.g. means, s.d., percentiles)
##
## Author: Claire Morton
##
## Date Created: 2022-12-31
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
library(rstatix)
library(cowplot)

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(total_race_eth, nonhispanic_white, pop_density, 
                over_200_percent_poverty, total_poverty, 
                total_LEP, LEP,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty,
         hispanic_or_nonwhite_prop = (total_race_eth-nonhispanic_white)/total_race_eth,
         lep_prop = LEP/total_LEP) %>%
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, nonhispanic_white, LEP, total_LEP))

# Find percentile of 35% under poverty level in California ---------------------------
mean(data$poverty_prop <= .35, na.rm = T)

# Find number of people and block groups meeting poverty 
# threshold alone (for trivariate metric) ---------------------------
poverty_alone <- data %>%
  filter(poverty_prop >= .35 &
           (is.na(hispanic_or_nonwhite_prop) | hispanic_or_nonwhite_prop < .75) &
           (is.na(lep_prop) | lep_prop < .4))
n_poverty_alone <- nrow(poverty_alone)
n_trivariate <- nrow(data %>% filter(univariate_dac == 1))
n_poverty_alone/n_trivariate

# Find number of people and block groups not meeting poverty 
# threshold (for trivariate metric) ---------------------------
not_poverty <- data %>%
  filter(poverty_prop < .35 &
           univariate_dac == 1)
n_not_poverty <- nrow(not_poverty)
n_trivariate <- nrow(data %>% filter(univariate_dac == 1))
n_not_poverty/n_trivariate
