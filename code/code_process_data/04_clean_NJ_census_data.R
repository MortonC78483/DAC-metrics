## ---------------------------
##
## Script name: 04_clean_NJ_census_data
##
## Purpose of script:
##    Import and set up block-group level univariate designations for New Jersey
##
## Author: Claire Morton
##
## Date Created: 2022-11-10
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

# Load data
block_group <- get_acs(
  geography = "block group",
  variables = c("B02001_001", "B02001_002", "B02001_003", "B02001_004", 
                "B02001_005", "B02001_006", "B02001_007", "B02001_008",
                "B03001_003", "B03002_003",
                "C17002_001", "C17002_008",
                "C16002_001", "C16002_004", "C16002_007", "C16002_010",
                "C16002_013"),
  year = 2020,
  state = "NJ"
) %>% 
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename("TotalRaceEth" = "B02001_001", 
         "White" = "B02001_002", 
         "Black" = "B02001_003",
         "AmericanIndianAlaskaNative" = "B02001_004", 
         "Asian" = "B02001_005", 
         "NativeHawaiianPacificIslander" = "B02001_006",
         "Other" = "B02001_007", 
         "TwoOrMore" = "B02001_008",
         "Hispanic" = "B03001_003",
         "NonHispanicWhite" = "B03002_003",
         "TotalPoverty" = "C17002_001",
         "Over200PercentPoverty" = "C17002_008",
         "TotalLEP" = "C16002_001",
         "LEP_1" = "C16002_004",
         "LEP_2" = "C16002_007",
         "LEP_3" = "C16002_010",
         "LEP_4" = "C16002_013") %>%
  mutate(LEP = rowSums(select(., starts_with("LEP")))) %>%
  select(-c(LEP_1, LEP_2, LEP_3, LEP_4)) %>%
  # Create race/ethnicity metric following CES 1.0, calculate percentile
  mutate(propHispanicOrNonWhite = (TotalRaceEth - NonHispanicWhite) / TotalRaceEth,
         percentile_raceeth = percent_rank(propHispanicOrNonWhite),
         propPoverty = (TotalPoverty - Over200PercentPoverty) / TotalPoverty,
         percentile_poverty = percent_rank(propPoverty),
         propLEP = (LEP) / TotalLEP,
         percentile_LEP = percent_rank(propLEP)) 
# create univariate DAC designation (actual New Jersey law thresholds)
block_group <- block_group %>%
  mutate(univariate_DAC = ifelse(((!is.na(propHispanicOrNonWhite) & propHispanicOrNonWhite >= .40) | 
                                    (!is.na(propPoverty) & propPoverty >= .35) | 
                                    (!is.na(propLEP) & propLEP >= .40)), 1, 0))
# Write data ------------------------------------------------------
write_csv(block_group, "data/data_interim/NJ_census_block_group.csv")
