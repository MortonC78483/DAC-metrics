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
library(tidycensus)

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty,
         hispanic_or_nonwhite_prop = (total_race_eth-nonhispanic_white)/total_race_eth,
         lep_prop = LEP/total_LEP) %>%
  dplyr::select(-c(over_200_percent_poverty, total_poverty, nonhispanic_white, LEP, total_LEP))

# Find number of people covered ---------------------------
sum(data$total_race_eth)

# Find number of Census Tracts covered ---------------------------
length(unique(data$GEOID))
  
# Find percentile of 35% under poverty level in California ---------------------------
mean(data$poverty_prop <= .35, na.rm = T)

# Find number of people and tracts meeting 
# poverty threshold alone (for trivariate metric) ---------------------------
poverty_alone <- data %>%
  filter(poverty_prop >= .35 &
           (is.na(hispanic_or_nonwhite_prop) | hispanic_or_nonwhite_prop < .75) &
           (is.na(lep_prop) | lep_prop < .4))
n_poverty_alone <- nrow(poverty_alone)
n_trivariate <- nrow(data %>% filter(univariate_dac == 1))
n_poverty_alone/n_trivariate # proportion of tracts
n_poverty_alone # number of tracts
sum(poverty_alone$total_race_eth) # number of people

# LEP threshold alone (for trivariate metric) ---------------------------
lep_alone <- data %>%
  filter(lep_prop >= .4 &
           (is.na(hispanic_or_nonwhite_prop) | hispanic_or_nonwhite_prop < .75) &
           (is.na(poverty_prop) | poverty_prop < .35))
n_lep_alone <- nrow(lep_alone)
n_trivariate <- nrow(data %>% filter(univariate_dac == 1))
n_lep_alone/n_trivariate

# Hispanic or nonwhite threshold alone (for trivariate metric) ---------------------------
hisp_nonwhite_alone <- data %>%
  filter(hispanic_or_nonwhite_prop >= .75 &
           (is.na(lep_prop) | lep_prop < .4) &
           (is.na(poverty_prop) | poverty_prop < .35))
n_hisp_nonwhite_alone <- nrow(hisp_nonwhite_alone)
n_trivariate <- nrow(data %>% filter(univariate_dac == 1))
n_hisp_nonwhite_alone/n_trivariate

# Find number of people and tracts not meeting poverty 
# threshold (for trivariate metric) ---------------------------
not_poverty <- data %>%
  filter(poverty_prop < .35 &
           univariate_dac == 1)
n_not_poverty <- nrow(not_poverty)
n_trivariate <- nrow(data %>% filter(univariate_dac == 1))
n_not_poverty/n_trivariate

# Find proportion of areas classified as disadvantaged that are urban areas
# 1000 people per square mile = 386.1 people per square kilometer
# This table appears in the paper
(data[data$pop_density >= 386.1,] %>%
  select(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  colSums(na.rm = T))/(data %>% select(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>% colSums(na.rm = T))

# Find number of new tracts and people classified as living in 
# disadvantaged communities with CES+ compared to CES
new_tracts <- data %>%
  filter(ces_dac_adj == 1) %>%
  filter(ces_dac == 0)

sum(new_tracts$total_race_eth) # number of new people classified as living in DACs

new_tracts <- new_tracts %>%
  unique() 

nrow(new_tracts) # number of new tracts

new_tracts$tract <- substr(new_tracts$GEOID, 1, 5)
table(substr(new_tracts$tract,3,5)) # number of new tracts in CES+, by county FIPS code 
                                    # (https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tiger2006se/app_a03.pdf)

# Trivariate mean/s.d. of Black and Hispanic Residents ---------------------------
triv <- data %>%
  filter(univariate_dac == 1) %>%
  mutate(hispanic = hispanic/total_race_eth,
         Black = Black/total_race_eth) %>%
  select(hispanic, Black)
mean(triv$hispanic)
sd(triv$hispanic)
mean(triv$Black)
sd(triv$Black)

# CEJST mean/s.d. of Black and Hispanic Residents ---------------------------
cejst <- data %>%
  filter(cejst_dac == 1) %>%
  mutate(hispanic = hispanic/total_race_eth,
         Black = Black/total_race_eth) %>%
  select(hispanic, Black)
mean(cejst$hispanic, na.rm = T)
sd(cejst$hispanic, na.rm = T)
mean(cejst$Black, na.rm = T)
sd(cejst$Black, na.rm = T)

# Range of Black and Hispanic residents in CES, CES+, EJI  ---------------------------
ces <- data %>%
  filter(ces_dac == 1) %>%
  mutate(hispanic = hispanic/total_race_eth,
         Black = Black/total_race_eth) %>%
  select(hispanic, Black)
mean(ces$hispanic, na.rm = T)
mean(ces$Black, na.rm = T)

ces_ <- data %>%
  filter(ces_dac_adj == 1) %>%
  mutate(hispanic = hispanic/total_race_eth,
         Black = Black/total_race_eth) %>%
  select(hispanic, Black)
mean(ces_$hispanic, na.rm = T)
mean(ces_$Black, na.rm = T)

eji <- data %>%
  filter(eji_dac == 1) %>%
  mutate(hispanic = hispanic/total_race_eth,
         Black = Black/total_race_eth) %>%
  select(hispanic, Black)
mean(eji$hispanic, na.rm = T)
mean(eji$Black, na.rm = T)
