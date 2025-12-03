## ---------------------------
##
## Script name: figure_01
##
## Purpose of script:
##    Create table 1, land area, number of block groups, and number of people
##    classified in all metrics
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
library(sf)
library(stargazer) # for table exporting

# function to append proportion to vector of numbers
add_perc <- function(data, denom) {
  return(paste0(data, " (", round(data/denom, 3), ")"))
}

# Load data ---------------------------
metrics_tract <- read.csv("data/data_processed/metrics_tract.csv")%>%
  mutate(GEOID = paste0("0", GEOID)) %>%
  dplyr::select(GEOID, total_race_eth, ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac)

tract <- tracts(
  state = "CA",
  year = 2019) %>%
  dplyr::select(ALAND, GEOID, geometry) %>%
  mutate(ALAND = 3.86102e-7 * ALAND) # convert from square meters to square miles

# Merge data ---------------------------
merged <- merge(metrics_tract, tract, by = "GEOID")

# Land area ---------------------------
land_area <- merged %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac, ALAND) %>%
  mutate(across(c(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac), ~ . * ALAND)) %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac) %>%
  colSums(na.rm = T) %>%
  round(-2) %>%
  add_perc(sum(tract$ALAND))

# Number of block groups ---------------------------
# sum number of DAC block groups in all metrics (exclude ID)
n_tracts <- merged %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac) %>%
  colSums(na.rm = T) %>%
  add_perc(nrow(metrics_tract))
  
# Number of people ---------------------------
n_people <- merged %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac, total_race_eth) %>%
  mutate(across(c(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac), ~ . * total_race_eth)) %>%
  dplyr::select(ces_dac, ces_dac_adj, univariate_dac, eji_dac, cejst_dac) %>%
  colSums(na.rm = T) %>%
  round(-2) %>%
  add_perc(sum(metrics_tract$total_race_eth))

# Join into table ---------------------------
row_names <- c("CES", "CES+", "Trivariate", "EJI", "CEJST")
table_01 <- data.frame(row_names, land_area, n_tracts, n_people)
colnames(table_01) = c("Metric", 
                       "Land area (proportion)", 
                       "Number of tracts (proportion)", 
                       "Number of people (proportion)")

stargazer(table_01, 
          summary=FALSE, rownames=FALSE,
          type = "html", out = "outputs/tables/table_01.html")


