## ---------------------------
##
## Script name: 01_clean_CA_census_data
##
## Purpose of script:
##    Load and clean 2019 ACS data; Census Tract-level data on race and ethnicity
##    and block group-level data on poverty, race/ethnicity, and linguistic 
##    isolation for California. Creates block-group level univariate DAC designation
##    based on thresholds identified by community partner.
##    Load and clean 2010 Census data; block-level data on block populations for 
##    California.
##
## Author: Claire Morton
##
## Date Created: 2022-09-28
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
library(sf)
library(tidycensus)
library(tigris)
source("api_keys.R") # creates api_key with key

census_api_key(api_key, install = TRUE, overwrite = TRUE)

# Tract-level data on race and ethnicity ---------------------------
tract <- get_acs(
  geography = "tract",
  variables = c("B02001_001", "B02001_002", "B02001_003", "B02001_004", 
                "B02001_005", "B02001_006", "B02001_007", "B02001_008",
                "B03002_012", "B03002_003"),
  year = 2019,
  state = "CA"
) %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename("Total" = "B02001_001", 
         "White" = "B02001_002", 
         "Black" = "B02001_003",
         "AmericanIndianAlaskaNative" = "B02001_004", 
         "Asian" = "B02001_005", 
         "NativeHawaiianPacificIslander" = "B02001_006",
         "Other" = "B02001_007", 
         "TwoOrMore" = "B02001_008",
         "Hispanic" = "B03002_012",
         "NonHispanicWhite" = "B03002_003"
         ) %>%
  # Create race/ethnicity metric following CES 1.0, calculate percentile
  mutate(propHispanicOrNonWhite = (Total - NonHispanicWhite) / Total,
         percentile = ifelse(Total == 0, NA, 0))  # set to be NA if there is no population

tract_nonzero <- tract %>%
  filter(propHispanicOrNonWhite != 0, Total != 0) %>%
  mutate(percentile = percent_rank(propHispanicOrNonWhite))

tract_zero <- tract %>%
  filter(propHispanicOrNonWhite == 0 | Total == 0)

tract <- rbind(tract_nonzero, tract_zero)
write_csv(tract, "data/data_interim/CA_census_tract_race_eth.csv")

# Block group-level data on race/ethnicity, poverty, linguistic isolation  ---------------------------
block_group <- get_acs(
  geography = "block group",
  variables = c("B02001_001", "B03002_003", "B03002_004", "B03002_005",
                "B03002_006", "B03002_007", "B03002_008", "B03002_009",
                "B03002_012", "B03002_003",
                "B02001_002", 
                "B02001_003",
                "B02001_004", 
                "B02001_005", 
                "B02001_006",
                "B02001_007", 
                "B02001_008",
                "C17002_001", "C17002_008",
                "C16002_001", "C16002_004", "C16002_007", "C16002_010",
                "C16002_013"),
  year = 2019,
  state = "CA"
) %>% 
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename("total_race_eth" = "B02001_001", 
         "nonhispanic_white" = "B03002_003", 
         "nonhispanic_black" = "B03002_004",
         "nonhispanic_american_indian_alaska_native" = "B03002_005", 
         "nonhispanic_asian" = "B03002_006", 
         "nonhispanic_native_hawaiian_pacific_islander" = "B03002_007",
         "nonhispanic_other" = "B03002_008", 
         "nonhispanic_two_or_more" = "B03002_009",
         "White" = "B02001_002", 
         "Black" = "B02001_003",
         "AmericanIndianAlaskaNative" = "B02001_004", 
         "Asian" = "B02001_005", 
         "NativeHawaiianPacificIslander" = "B02001_006",
         "Other" = "B02001_007", 
         "TwoOrMore" = "B02001_008",
         "hispanic" = "B03002_012",
         "total_poverty" = "C17002_001",
         "over_200_percent_poverty" = "C17002_008",
         "total_LEP" = "C16002_001",
         "LEP_1" = "C16002_004",
         "LEP_2" = "C16002_007",
         "LEP_3" = "C16002_010",
         "LEP_4" = "C16002_013") %>%
  mutate(LEP = rowSums(select(., starts_with("LEP")))) %>%
  select(-c(LEP_1, LEP_2, LEP_3, LEP_4)) %>%
  # Create race/ethnicity metric following CES 1.0, calculate percentile
  mutate(prop_hispanic_or_nonwhite = (total_race_eth - nonhispanic_white) / total_race_eth,
         percentile_raceeth = percent_rank(prop_hispanic_or_nonwhite),
         prop_poverty = (total_poverty - over_200_percent_poverty) / total_poverty,
         percentile_poverty = percent_rank(prop_poverty),
         prop_LEP = (LEP) / total_LEP,
         percentile_LEP = percent_rank(prop_LEP)) 

# load tigris shapefiles
block_group_sf <- tigris::block_groups(state = "CA", year = 2019) %>%
  select(GEOID, ALAND) %>%
  st_drop_geometry()

# join shapefile to block group dataset
block_group <- merge(block_group, block_group_sf) %>%
  mutate(pop_density = total_race_eth/ALAND) %>%
  select(-ALAND)

# create univariate DAC designation
block_group <- block_group %>%
  mutate(univariate_dac = ifelse(((!is.na(prop_hispanic_or_nonwhite) & prop_hispanic_or_nonwhite >= .75) | 
                                    (!is.na(prop_poverty) & prop_poverty >= .35) | 
                                    (!is.na(prop_LEP) & prop_LEP >= .4)), 1, 0))
write_csv(block_group, "data/data_interim/CA_census_block_group.csv")

# Block-level data on population -- delete all unpopulated blocks in a shapefile ---------------------------
block <- read_sf("data/block_data/tabblock2010_06_pophu/tabblock2010_06_pophu.shp")
block_populated <- block %>%
  filter(POP10 != 0) %>%
  mutate(GEOID = paste0(STATEFP10, COUNTYFP10, TRACTCE10)) %>%
  select(GEOID, geometry) # get geometry of block and tract block is in

st_write(block_populated, "data/data_interim/census_block_populated/CA_census_block_populated.shp")


