## ---------------------------
##
## Script name: figure_cafo_priority
##
## Purpose of script:
##    Create table showing how 
##
## Author: Claire Morton
##
## Date Created: 2025-12-31
##
## Email: mortonc@stanford.edu
##
## ---------------------------
##
## Notes:
##    Figures similar to this national-level analysis:
##    https://grist.org/equity/climate-and-economic-justice-screening-tool-race/ 
##
## ---------------------------

# Install packages ---------------------------
library(readr)
library(tidyverse)
library(ggplot2) 
library(ggpubr)

# Load data
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  dplyr::select(GEOID,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)
cafo_data <- read_csv("data/data_interim/census_tract_cafos.csv") %>%
  dplyr::select(-`...1`)

merged <- merge(data, cafo_data)

# get tool proportion of CAFO-exposed tracts classified as DACs
cafo_prob = merged %>% 
  dplyr::filter(scaled_cafo_score != 0) %>%
  summarize(mean_eji = mean(eji_dac, na.rm = T),
            mean_cejst = mean(cejst_dac),
            mean_ces = mean(ces_dac, na.rm = T),
            mean_univar = mean(univariate_dac),
            mean_ces_adj = mean(ces_dac_adj, na.rm = T))

gen_prob = merged %>%
  summarize(mean_eji = mean(eji_dac, na.rm = T),
            mean_cejst = mean(cejst_dac),
            mean_ces = mean(ces_dac, na.rm = T),
            mean_univar = mean(univariate_dac),
            mean_ces_adj = mean(ces_dac_adj, na.rm = T))

x <- rbind(cafo_prob, cafo_prob/gen_prob)

x <- x[, c("mean_cejst",
           "mean_ces",
           "mean_ces_adj",
           "mean_eji",
           "mean_univar")]

colnames(x) <- c("CEJST", "CES", "CES+", "EJI", "Trivariate")

formatted <- apply(x, 2, function(col) {
  sprintf("%.3f (%.3f)", col[1], col[2])
})

formatted
