## ---------------------------
##
## Script name: 04_kappa
##
## Purpose of script:
##    This script calculates Kappa statistics to compare land area classified as disadvantaged communities in the 
##    metrics, outputting a table of Kappa statistics for all metric pairings.
##
## Author: Claire Morton
##
## Date Created: 2023-02-08
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
library(tigris)
library(sf)

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  dplyr::select(GEOID, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)
# load tigris shapefiles
tract_sf <- tigris::tracts(state = "CA", year = 2019) %>%
  dplyr::select(GEOID, ALAND) %>%
  st_drop_geometry()
data <- merge(data, tract_sf)

# Function to calculate kappa statistic ---------------------------
calc_kappa <- function(var_1, var_2) {
  total_land = sum(data$ALAND)
  pii = sum(data[data[[var_1]] == 1 & data[[var_2]] == 1,]$ALAND, na.rm = T)/total_land
  p_i = sum(data[data[[var_2]]== 1,]$ALAND, na.rm = T)/total_land
  pi_ = sum(data[data[[var_1]] == 1,]$ALAND, na.rm = T)/total_land
  d = 1-pi_-p_i+pii
  k = (pii-pi_*p_i)/((pi_+p_i)/2-pi_*p_i)
  return(k)
}

# Perform calculations of kappa statistic ---------------------------
metric_names = c("univariate_dac", "ces_dac", "ces_dac_adj", "eji_dac", "cejst_dac")
data_frame = data.frame(matrix(ncol = 3, nrow = 0))
colnames(data_frame) = c("metric1", "metric2", "khat")
for (i in 1:(length(metric_names)-1)) {
  for (j in (i+1):length(metric_names)) {
    data_frame[nrow(data_frame) + 1,] = c(metric_names[i], metric_names[j], calc_kappa(metric_names[i], metric_names[j]))
  }
}
data_frame

# Format table for export ---------------------------
table_kappa <- matrix(nrow = length(metric_names), ncol = length(metric_names), dimnames = list(metric_names, metric_names))
for (i in 1:(length(metric_names)-1)){ # row
  for (j  in (i+1):length(metric_names)){ # column
    val = (data_frame %>%
      filter(data_frame$metric1 == metric_names[i] & data_frame$metric2 == metric_names[j]))$khat
    table_kappa[i, j] = val
  }
}

write.csv(table_kappa, "data/data_interim/table_kappa.csv")

