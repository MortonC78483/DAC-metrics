## ---------------------------
##
## Script name: 07_assess_2-3_coverage
##
## Purpose of script:
##    Run simulations to create a distribution of (assuming all tools are randomly 
##    distributed) how many block groups we expect to be covered by 4 or 5 tools and 
##    compare to what we observe.
##
## Author: Claire Morton
##
## Date Created: 2023-02-21
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

# Functions ---------------------------
# function takes a dataset with column names "tract" (tract GEOIDs) and "GEOID" (block group GEOIDs),
# along with a list of indices representing indices of tracts of interest (1 indexed). Returns
# list of all block group GEOIDs in those tracts
extract_block_groups <- function(data, indices){
  tract_geoids <- data[indices, 1]
  return(data[data$tract %in% tract_geoids, 2])
}

# function takes a dataset with column "tract" (tract GEOIDs) and "GEOID" (block group GEOIDs),
# along with the number of tracts/block groups designated as DACs by each of the five metrics and a 
# vector of number of tools designating an area as disadvantaged (0-5) we're interested in.
# Returns the number of block groups marked as DACs by 2 or 3 metrics in a random simulation.
get_number_bg <- function(tract_bg_geoid, trivariate, ces, ces_adj, eji, cejst, vec){
  # pull GEOIDs of tracts/block groups
  trivariate_sim <- sample(1:length(unique(tract_bg_geoid$GEOID)), trivariate, replace = FALSE)
  ces_sim <- sample(1:length(unique(tract_bg_geoid$tract)), ces, replace = FALSE)
  ces_adj_sim <- sample(1:length(unique(tract_bg_geoid$tract)), ces_adj, replace = FALSE)
  eji_sim <- sample(1:length(unique(tract_bg_geoid$tract)), eji, replace = FALSE)
  cejst_sim <- sample(1:length(unique(tract_bg_geoid$tract)), cejst, replace = FALSE)
  
  # pull block group GEOIDs from tracts
  trivariate_geoid <- tract_bg_geoid[trivariate_sim, 2] # selects all block group GEOIDs in selected block groups
  ces_geoid <- extract_block_groups(tract_bg_geoid, ces_sim)
  ces_adj_geoid <- extract_block_groups(tract_bg_geoid, ces_adj_sim)
  eji_geoid <- extract_block_groups(tract_bg_geoid, eji_sim)
  cejst_geoid <- extract_block_groups(tract_bg_geoid, cejst_sim)
  
  # Assess overlap between GEOIDs (number of GEOIDs in common)
  all_geoids <- table(c(trivariate_geoid, ces_geoid, ces_adj_geoid, eji_geoid, cejst_geoid))
  #length(all_geoids[all_geoids==4])+length(all_geoids[all_geoids == 5]) # the number of block groups covered by 2 or three metrics
  length(all_geoids[all_geoids%in%vec])
}

# Function that takes a dataset with column "tract" (tract GEOIDs) and "GEOID" (block group GEOIDs),
# along with the number of tracts/block groups designated as DACs by each of the five metrics, and the number of draws to simulate,
# and the number to calculate and return the percentile of from the simulated vector, and the vector of number of tools covering an area
# that we're interested in.
# Simulates n random draws of the number of block groups covered by 2 or 3 metrics, and calculates the percentile of the passed
# number.
simulate_percentile <- function(tract_bg_geoid, trivariate, ces, ces_adj, eji, cejst, n = 100, num_covered, vec){
  simulated_nums = c()
  for (i in 1:n) {
    simulated_nums = append(simulated_nums, get_number_bg(tract_bg_geoid, trivariate, ces, ces_adj, eji, cejst, vec))
  }
  
  return(sum(simulated_nums <= num_covered)/length(simulated_nums))
}

# Load data ---------------------------
data <- read.csv("data/data_processed/metrics_block_group.csv") %>%
  select(GEOID, tract, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac, n_dac)

# Count how many tracts/block groups per tool ---------------------------
# Trivariate (block group)
trivariate <- data %>% select(GEOID, tract, univariate_dac) %>%
  filter(univariate_dac == 1) %>%
  nrow()

# CES (tract)
ces <- data %>% select(tract, ces_dac) %>%
  filter(ces_dac == 1) %>%
  unique() %>%
  nrow()

# CES+ (tract)
ces_adj <- data %>% select(tract, ces_dac_adj) %>%
  filter(ces_dac_adj == 1) %>%
  unique() %>%
  nrow()

# EJI (tract)
eji <- data %>% select(tract, eji_dac) %>%
  filter(eji_dac == 1) %>%
  unique() %>%
  nrow()

# CEJST (tract)
cejst <- data %>% select(tract, cejst_dac) %>%
  filter(cejst_dac == 1) %>%
  unique() %>%
  nrow()

# Create dataset of tract and block group GEOIDs ---------------------------
tract_bg_geoid <- data %>% select(tract, GEOID)

# Simulate random draws of appropriate numbers of tracts/GEOIDs (with set seed) ---------------------------
set.seed(NULL)
set.seed(10)

num_4_5_covered <- data %>%
  filter(n_dac %in% c(4,5)) %>%
  nrow()
perc <- simulate_percentile(tract_bg_geoid, trivariate, ces, ces_adj, eji, cejst, 1000, num_4_5_covered, c(4,5))
perc > .975

# num_0_1_covered <- data %>%
#   filter(n_dac %in% c(0,1)) %>%
#   nrow()
# ex <- simulate_percentile(tract_bg_geoid, trivariate, ces, ces_adj, eji, cejst, 100, num_0_1_covered, c(0,1))
# 
# num_2_3_covered <- data %>%
#   filter(n_dac %in% c(2,3)) %>%
#   nrow()
# ex <- simulate_percentile(tract_bg_geoid, trivariate, ces, ces_adj, eji, cejst, 100, num_2_3_covered, c(2,3))



