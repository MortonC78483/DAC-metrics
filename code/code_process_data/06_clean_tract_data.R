## ---------------------------
##
## Script name: 06_clean_tract_data
##
## Purpose of script:
##    This script creates a tract level dataset.
##
## Author: Claire Morton
##
## Date Created: 2022-12-04
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
library(tidyverse)

# Load data ---------------------------
tract <- read_csv("data/data_interim/CA_census_tract.csv")
ces_scores <- read_csv("data/data_interim/recalculated_ces_scores.csv")
eji_scores <- read_csv("data/data_raw/EJI_data/DataRecords.csv")
cejst_scores <- read_csv("data/data_raw/CEJST_data/1.0-communities.csv")

# Clean CEJST data ---------------------------
cejst_scores <- cejst_scores %>%
  filter(`State/Territory` == "California")

# Correct for incorrect Census tract
cejst_to_change <- cejst_scores %>% 
  filter(cejst_scores$`Census tract 2010 ID` == "06037930401")
cejst_to_change["Census tract 2010 ID"] <- "06037137000"
cejst_scores <- cejst_scores %>%
  filter(cejst_scores$`Census tract 2010 ID` != "06037930401")
cejst_scores <- rbind(cejst_scores, cejst_to_change)

# Clean data
cejst_scores <- cejst_scores %>%
  select(`Census tract 2010 ID`, `Identified as disadvantaged`)
colnames(cejst_scores) = c('GEOID', 'cejst_dac')

# Clean EJI data ---------------------------
# Note the EJI data does not have the incorrect census tract, so a correction is not needed
eji_scores <- eji_scores %>%
  select(geoid, SPL_EJI, RPL_EJI)
colnames(eji_scores) <- c('GEOID', 'eji_score', 'eji_perc')

# Join CES scores to block group data ---------------------------
ces_scores <- ces_scores %>% 
  select(GEOID, 
         cleanup_sites, groundwater_threats, haz_waste, imp_water_bodies, solid_waste,
         ces_score, ces_score_adj,
         ces_perc, ces_perc_adj)


# Create indicator column for which metrics a block group qualifies based on, and another for how many
merged <- merge(tract, ces_scores, by.x = "GEOID", by.y = "GEOID") %>%
  merge(cejst_scores, by.x = "GEOID", by.y = "GEOID", all = T) %>%
  merge(eji_scores, by.x = "GEOID", by.y = "GEOID", all = T) %>%
  mutate(ces_dac = ifelse(ces_perc > 75, 1, 0),
         ces_dac_adj = ifelse(ces_perc_adj > 75, 1, 0),
         eji_dac = ifelse(eji_perc > .75, 1, 0),
         n_dac = rowSums(cbind(ces_dac, ces_dac_adj, univariate_dac, cejst_dac, eji_dac), na.rm = T)) %>%
  select(GEOID, total_race_eth, 
         nonhispanic_white, nonhispanic_black, nonhispanic_american_indian_alaska_native, 
         nonhispanic_asian, nonhispanic_native_hawaiian_pacific_islander, nonhispanic_other, 
         nonhispanic_two_or_more, hispanic, 
         White, Black, AmericanIndianAlaskaNative, Asian, NativeHawaiianPacificIslander, Other, TwoOrMore,
         total_poverty, over_200_percent_poverty, total_LEP, LEP, pop_density,
         cleanup_sites, groundwater_threats, haz_waste, imp_water_bodies, solid_waste, 
         ces_score, ces_perc, ces_score_adj, ces_perc_adj, eji_score, eji_perc,
         univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac, n_dac
         ) %>%
  mutate(cejst_dac = ifelse(cejst_dac, 1, 0))

# Export dataset ---------------------------
write_csv(merged, "data/data_processed/metrics_tract.csv")

