## ---------------------------
##
## Script name: figure_05
##
## Purpose of script:
##    Create figure of proportion of people of various racial/ethnic groups in DACs by metric
##
## Author: Claire Morton
##
## Date Created: 2022-01-11
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
library(cowplot)
library(MASS) 
library(reshape2) 

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  dplyr::select(nonhispanic_white, nonhispanic_black, nonhispanic_american_indian_alaska_native, 
                nonhispanic_asian, nonhispanic_native_hawaiian_pacific_islander, 
                nonhispanic_other, nonhispanic_two_or_more, hispanic,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)%>%
  mutate(nonhispanic_other= nonhispanic_other+nonhispanic_two_or_more) %>%
  dplyr::select(-nonhispanic_two_or_more)

# Format data ---------------------------
# Create dataset with DAC designation type and variables of interest
univariate_dac_data <- data %>%
  dplyr::filter(univariate_dac == 1) %>%
  mutate(dac = "univariate")
ces_dac_data <- data %>%
  dplyr::filter(ces_dac == 1) %>%
  mutate(dac = "ces")
ces_dac_adj_data <- data %>%
  dplyr::filter(ces_dac_adj == 1) %>%
  mutate(dac = "ces_adj")
eji_dac_data <- data %>%
  dplyr::filter(eji_dac == 1) %>%
  mutate(dac = "eji")
cejst_dac_data <- data %>%
  dplyr::filter(cejst_dac == 1) %>%
  mutate(dac = "cejst")
total_data <- data %>%
  mutate(dac = "total")

dac_data <- rbind(univariate_dac_data, ces_dac_data, ces_dac_adj_data, eji_dac_data, cejst_dac_data, total_data) %>%
  dplyr::select(-c(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac))


# create table
table <- dac_data %>%
  group_by(dac) %>%
  summarise(across(everything(), sum))
ex <- melt(table, id = "dac") %>%
  group_by(dac) %>%
  mutate(sum = sum(value))

figure_s3 <- ggplot(ex, aes(fill=variable, y=value, x=dac)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_x_discrete(limits = c("total", "univariate", "eji", "ces_adj", "ces",  "cejst"), 
                   labels = c("Total", "Trivariate", "EJI", "CES+", "CES", "CEJST")) +
  scale_fill_brewer(type = "qual", palette = 8, 
                    name = "Race/Ethnicity",
                    labels = c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic American \nIndian/Alaska Native",
                               "Non-Hispanic Asian", "Non-Hispanic Native \nHawaiian/Pacific Islander", "Non-Hispanic Other",
                               "Hispanic"))+
  xlab("Tool")+
  ylab("Proportion Residents in Disadvantaged Communities")+
  theme_classic()

# Export ---------------------------
cowplot::ggsave2("outputs/figures/fig_s03.png", figure_s3,
                 width = 7,
                 height = 4,
                 units = c("in"))  
