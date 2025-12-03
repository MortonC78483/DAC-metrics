## ---------------------------
##
## Script name: figure_04
##
## Purpose of script:
##    Create figure 4, proportion of people of various racial/ethnic groups in DACs by metric
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
  dplyr::select(nonhispanic_white, Black, AmericanIndianAlaskaNative, 
                Asian, NativeHawaiianPacificIslander, 
                Other, TwoOrMore, hispanic,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)%>%
  mutate(Other= Other+TwoOrMore) %>%
  dplyr::select(-TwoOrMore)

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
totals <- ex %>%
  filter(dac == "total")

# label names
new.labs <- c("Non-Hispanic White", "Hispanic", 
              "Black", "American Indian/Alaska Native",
              "Asian", "Native Hawaiian/Pacific Islander",
              "Other")
names(new.labs) <- c("nonhispanic_white", "hispanic", 
                     "Black", "AmericanIndianAlaskaNative",
                     "Asian", "NativeHawaiianPacificIslander",
                     "Other")

figure_5 <- ggplot(ex, aes(fill = variable, y=value/sum, x=dac)) + 
  geom_bar(stat="identity") +
  geom_hline(data = totals, aes(yintercept = value/sum), linetype = 2, color = "black")+
  facet_wrap(~variable, scales ="free", labeller = labeller(variable = new.labs))+
  coord_flip() +
  scale_x_discrete(limits = c("univariate", "eji", "cejst", "ces_adj", "ces"),
                   labels = c("Trivariate", "EJI", "CEJST", "CES+", "CES")) +
  geom_vline(data = totals, aes(xintercept = value/sum))+
  scale_fill_brewer(type = "qual", palette = 8)+
  theme_classic()+
  ylab("Proportion of Residents of Disadvantaged \nCommunities by Race/Ethnicity")+
  xlab("Tool")+
  theme(legend.position = "none")

# Export ---------------------------
cowplot::ggsave2("outputs/figures/fig_04.png", figure_5,
                 width = 8,
                 height = 6,
                 units = c("in"))  
