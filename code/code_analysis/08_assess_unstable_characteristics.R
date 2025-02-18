## ---------------------------
##
## Script name: 08_assess_unstable_characteristics
##
## Purpose of script:
##    Obtain block groups that are classified as DACs by only 2 or 3 metrics (the most "unstable")
##    and assess which metrics are most contributing to this and the characteristics of unstable block
##    groups compared to those rarely (0 or 1 tool) and often (4 or 5 tools) classified as DACs.
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

# Load data ---------------------------
data <- read.csv("data/data_processed/metrics_block_group.csv")  %>%
  #dplyr::select(total_race_eth, hispanic, Black, pop_density, over_200_percent_poverty, total_poverty,
  #              univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac, n_dac) %>%
  mutate(poverty_prop = (total_poverty - over_200_percent_poverty)/total_poverty,
         hispanic_prop = hispanic/total_race_eth,
         black_prop = Black/total_race_eth,
         n_dac_group = ifelse(n_dac %in% c(0, 1), "0 or 1", ifelse(n_dac %in% c(2,3), "2 or 3", "4 or 5"))) #%>%
  #dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic, Black))
  
#%>%
  #select(GEOID, tract, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac, n_dac)

# Make plots for characteristics of communities classified rarely, sometimes, and often ---------------------------
part_a <- ggplot(data, aes(x = factor(n_dac_group), y = black_prop, group = factor(n_dac_group))) +
  geom_violin() +
  ylab("Proportion of Black Residents") +
  xlab("Number of tools classifying as disadvantaged")

part_b <- ggplot(data, aes(x = factor(n_dac_group), y = poverty_prop, group = factor(n_dac_group))) +
  geom_violin() + 
  ylab("Proportion of Residents in Poverty") +
  xlab("Number of tools classifying as disadvantaged")

part_c <- ggplot(data, aes(x = factor(n_dac_group), y = hispanic_prop, group = factor(n_dac_group))) +
  geom_violin() + 
  ylab("Proportion of Hispanic Residents") +
  xlab("Number of tools classifying as disadvantaged")
  
ggarrange(part_a, part_b, part_c)

# Determine which tools are most often classifying communities marked by only 2 or 3 metrics as DACs ---------------------------
bg_2_or_3 <- data %>%
  filter(n_dac_group == "2 or 3") %>%
  select(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  colSums(na.rm = T)

# Get the proportion of unstable block group classifications for each tool vs. proportion of total block group classifications
# Greater than 1 indicates the tool tends to classify block groups that aren't often classified by the other tools (doesn't tend 
# to agree with the majority), less than 1 indicates the tool tends to classify block groups that are often classified by the 
# other tools (it tends to agree with the majority)
bg_2_or_3/sum(bg_2_or_3)

total_bg <- data %>% 
  select(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  colSums(na.rm = T)
total_bg/sum(total_bg)

(bg_2_or_3/sum(bg_2_or_3))/(total_bg/sum(total_bg))

# Get the proportion of a tool's DACs that are classified as DACs by only 2 or 3 metrics total.
# THIS IS THE TABLE CURRENTLY IN THE PAPER
bg_2_or_3/total_bg

# determine the proportion of DACs (from any tool) that are unstably designated
data2 <- data %>%
  filter(n_dac != 0)
sum(data2$n_dac_group == "2 or 3")
sum(data2$n_dac_group == "2 or 3")/nrow(data2)

# Determine which tools (of EJI, CEJST, CES) are most often classifying communities marked by only 1 or 2 of these metrics as DACs ---------------------------
bg_real_1_or_2 <- data %>%
  mutate(n_dac = ces_dac + cejst_dac+ eji_dac, 
         n_dac_group = ifelse(n_dac == 0, "0", ifelse(n_dac == 3, "3", "1 or 2"))) %>%
  filter(n_dac_group == "1 or 2") %>%
  select(ces_dac, eji_dac, cejst_dac) %>%
  colSums(na.rm = T)
bg_real_1_or_2/total_bg[c("ces_dac", "eji_dac", "cejst_dac")]


