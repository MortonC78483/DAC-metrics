## ---------------------------
##
## Script name: 02_anova
##
## Purpose of script:
##    This script determines, for block groups, whether DAC classifications from metrics (CES, CES
##    adjusted, CEJST, EJI, and New Jersey-like) result in different mean population density, hazards,
##    and below 2x federal poverty level. Run post-hoc Tukey test for significant ANOVA results
##
## Author: Claire Morton
##
## Date Created: 2022-12-07
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

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  dplyr::select(total_race_eth, hispanic, pop_density, over_200_percent_poverty, total_poverty,
    univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty - over_200_percent_poverty)/total_poverty,
         hispanic_prop = hispanic/total_race_eth) %>%
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic))

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

dac_data <- rbind(univariate_dac_data, ces_dac_data, ces_dac_adj_data, eji_dac_data, cejst_dac_data)

# Run population density ANOVA ---------------------------
# # Model population density as a function of DAC designation type
one.way <- aov(pop_density ~ dac, data = dac_data)
summary(one.way)

# Run post-hoc testing
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

# Plot population density
boxplot(dac_data$pop_density ~ dac_data$dac)

# Run poverty ANOVA ---------------------------
# Model poverty as a function of DAC designation type
one.way <- aov(poverty_prop ~ dac, data = dac_data)
summary(one.way)

# Run post-hoc testing
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

# Plot poverty
boxplot(dac_data$poverty_prop ~ dac_data$dac)

# Run proportion Hispanic/Latino ANOVA ---------------------------
# Model proportion Hispanic/Latino as a function of DAC designation type
one.way <- aov(hispanic_prop ~ dac, data = dac_data)
summary(one.way)

# Run post-hoc testing
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

# Plot proportion Hispanic/Latino
boxplot(dac_data$hispanic_prop ~ dac_data$dac)


