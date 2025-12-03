## ---------------------------
##
## Script name: clean_census_data
##
## Purpose of script:
##    Load CES scores and components for CES 4.0, load tract racial/ethnic characteristics, voting,
##    and oil wells/CAFO exposure scores. Recalculate CES 4.0 and export dataset of score components
##    and adjusted/final scores.
##
## Author: Claire Morton
##
## Date Created: 2022-10-02
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
library(readxl)

# Import data ---------------------------
wells_exposure <- read.csv("data/data_interim/census_tract_oil_wells.csv") %>%
  select(-X) %>%
  mutate(GEOID = paste0("0", as.character(GEOID)))

# cafos_exposure <- read.csv("data/data_interim/census_tract_cafos.csv") %>%
#   select(-X) %>%
#   mutate(GEOID = paste0("0", as.character(GEOID))) %>%
#   rename("CAFO Score" = "scaled_cafo_score",
#          "CAFO Pctl" = "percentile_cafo_score")

voting <- read.csv("data/data_interim/voting.csv") %>%
  select(-X) %>%
  mutate(GEOID = paste0("0", as.character(GEOID))) %>%
  rename("NonVoters Pctl" = "percentile",
         "propNonVoters" = "prop_nonvoters")

race_eth <- read.csv("data/data_interim/CA_census_tract_race_eth.csv") %>%
  mutate(GEOID = paste0("0", as.character(GEOID)))

ces <- read_excel("data/data_raw/CES_4.0_data/calenviroscreen40resultsdatadictionaryf2021/calenviroscreen40resultsdatadictionary_F_2021.xlsx") 
ces <- ces %>%
  mutate('Census Tract' = paste0("0", as.character(ces$'Census Tract')))

# fix tract numbering mistake in CES data
ces_to_change <- ces %>% 
  filter(ces$'Census Tract' == "06037930401")
ces_to_change["Census Tract"] <- "06037137000"
ces <- ces %>%
  filter(ces$'Census Tract' != "06037930401")
ces <- rbind(ces, ces_to_change)

# fix column data types in CES data
ces <- ces %>%
  mutate_at(vars('Lead', 'Lead Pctl', 'Low Birth Weight', 'Low Birth Weight Pctl',
                 'Education', 'Education Pctl', 'Linguistic Isolation', 
                 'Linguistic Isolation Pctl', 'Unemployment', 
                 'Unemployment Pctl', 'Housing Burden', 'Housing Burden Pctl'), 
            as.numeric)

# join race/ethnicity data, oil data, voting data, and CAFO data into CES data
race_eth_pctl <- race_eth %>%
  select(-NAME) %>%
  rename("Hispanic or NonWhite Pctl" = "percentile")
colnames(wells_exposure) <- c("Census Tract",
                              "Well Count",
                              "Well Count Pctl")

ces <- merge(ces, race_eth_pctl, by.x = "Census Tract", by.y = "GEOID") %>%
  merge(wells_exposure, by = "Census Tract") %>%
  merge(voting, by.x = "Census Tract", by.y = "GEOID", all = T)

# Adjust CES calculation  ---------------------------
ces_scores <- ces %>%
  mutate(`Exposure Adjusted` = rowMeans(subset(ces, 
                                    select = c('Ozone Pctl', 
                                               'PM2.5 Pctl', 
                                               'Diesel PM Pctl', 
                                               'Drinking Water Pctl', 
                                               'Lead Pctl', 
                                               'Pesticides Pctl',
                                               'Tox. Release Pctl', 
                                               'Traffic Pctl')), na.rm = TRUE),
         `Environmental Effects Adjusted` = rowMeans(subset(ces,
                                                 select = c('Cleanup Sites Pctl',
                                                            'Groundwater Threats Pctl',
                                                            'Haz. Waste Pctl',
                                                            'Imp. Water Bodies Pctl',
                                                            'Solid Waste Pctl',
                                                            'Well Count Pctl')), # adding oil well count and CAFOs
                                          na.rm = TRUE),
         `Sensitive Population Adjusted` = rowMeans(subset(ces, select = c('Asthma Pctl', 
                                                                'Cardiovascular Disease Pctl',
                                                                'Low Birth Weight Pctl')),
                                         na.rm = TRUE),
         `Socioeconomic Factor Adjusted` = rowMeans(subset(ces, select = c('Education Pctl',
                                                                'Linguistic Isolation Pctl',
                                                                'Housing Burden Pctl',
                                                                'Poverty Pctl',
                                                                'Unemployment Pctl',
                                                                'Hispanic or NonWhite Pctl',
                                                                'NonVoters Pctl')), # adding race and voters
                                         na.rm = TRUE))

# Recalculate CalEnviroScreen Score   ---------------------------
ces_scores <- ces_scores %>%
  mutate(`Pollution Burden Adjusted` = ifelse(is.na(ces_scores$`Pollution Burden`), 
                                   NA, ((ces_scores$`Exposure Adjusted` + 
                                           .5*(ces_scores$`Environmental Effects Adjusted`))/1.5)),
         `Pollution Burden Score Adjusted` = `Pollution Burden Adjusted`/max(`Pollution Burden Adjusted`, na.rm = TRUE)*10,
         `Pop. Char. Adjusted` = ifelse(is.na(ces_scores$`Pop. Char. Score`),
                    NA, ((ces_scores$`Sensitive Population Adjusted` + ces_scores$`Socioeconomic Factor Adjusted`)/2)),
         `Pop. Char. Score Adjusted` = `Pop. Char. Adjusted`/max(`Pop. Char. Adjusted`, na.rm = TRUE)*10,
         `CES Score Adjusted` = `Pollution Burden Score Adjusted` * `Pop. Char. Score Adjusted`,
         `CES Percentile Adjusted` = percent_rank(`CES Score Adjusted`)*100)

# Rename columns   ---------------------------
ces_scores <- ces_scores %>%
  select("Census Tract", 
         "Total Population", "White", "Black", "AmericanIndianAlaskaNative", 
         "Asian", "NativeHawaiianPacificIslander", "Other", "TwoOrMore", 
         "NonHispanicWhite", "Hispanic",
         "Cleanup Sites", "Groundwater Threats", "Haz. Waste", "Imp. Water Bodies", "Solid Waste",
         "CES 4.0 Score", "CES 4.0 Percentile", "CES Score Adjusted", "CES Percentile Adjusted")
colnames(ces_scores) = c("GEOID", "tot_pop", "white", "black", "american_indian_alaska_native",
                        "asian", "native_hawaiian_pacific_islander", "other", "two_or_more",
                        "nonhispanic_white", "hispanic", "cleanup_sites", "groundwater_threats",
                        "haz_waste", "imp_water_bodies", "solid_waste", "ces_score", "ces_perc",
                        "ces_score_adj", "ces_perc_adj")

write.csv(ces_scores, "data/data_interim/recalculated_ces_scores.csv")








