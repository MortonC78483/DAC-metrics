## ---------------------------
##
## Script name: 04_clean_voting_data.R
##
## Purpose of script:
##    Load raw voting data by census block group, aggregate to tract level voting data (proportion
##    of population over 18 that voted in 20212 and 2016 elections, averaged), rank percentiles of voter
##    participation, and export data.
##
## Author: Claire Morton
##
## Date Created: 2022-11-21
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

# Import data ---------------------------
voting <- readRDS("data/voting_data/voting_data_for_claire.rds")

# Clean data ---------------------------
# aggregate block groups to tracts
voting <- voting %>%
  mutate(tract = substr(GEOID, 1, 11)) %>%
  group_by(tract) %>%
  summarize(pop_total_over18 = sum(pop_total_over18),
            pop_nonvoters = sum(pop_nonvoters)) %>%
  mutate(prop_nonvoters = ifelse(pop_total_over18 == 0, 0, (pop_nonvoters/pop_total_over18))) %>%
  select(tract, prop_nonvoters) %>%
  rename("GEOID" = "tract")

# Append percentiles to dataframe  ---------------------------
voting_nonzero <- voting %>%
  filter(prop_nonvoters != 0) %>%
  mutate(percentile = percent_rank(prop_nonvoters))

voting_zero <- voting %>%
  filter(prop_nonvoters == 0) %>%
  mutate(percentile = 0)

voting_percentiles <- rbind(voting_nonzero, voting_zero)

# Export data ---------------------------
write.csv(voting_percentiles, "data/data_interim/voting.csv")


