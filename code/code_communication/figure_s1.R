## ---------------------------
##
## Script name: figure_s1
##
## Purpose of script:
##    Create figure s1, distributions of population density, poverty, and 
##    Hispanic/Latino people proportion in DACs for all metrics
##
## Author: Claire Morton
##
## Date Created: 2022-01-12
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

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(total_race_eth, pop_density, Black, over_200_percent_poverty, total_poverty, hispanic,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty,
         hispanic_prop = hispanic/total_race_eth,
         black_prop = Black/total_race_eth,
         pop_density = pop_density * 1000000) %>% # people/km^2
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic, Black))

# Colors ---------------------------
palette <- RColorBrewer::brewer.pal(5, "Dark2")
palette[1] = "#2caadb"

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

# Create figure components ---------------------------
part_a <- ggplot(dac_data%>% filter(dac!= "total"), aes(x = pop_density, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(name = "Screening Tools", values = palette)+
  geom_density(data = dac_data%>% filter(dac== "total"), aes(x = pop_density), color = "black")+
  ylab("") +
  xlab("Population Density\n (People/Square Kilometer)")
part_b <- ggplot(dac_data%>% filter(dac!= "total"), aes(x = poverty_prop, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(name = "Screening Tools", values = palette)+
  geom_density(data = dac_data%>% filter(dac== "total"), aes(x = poverty_prop), color = "black")+
  ylab("") +
  xlab("Proportion of Households\nin Poverty")
part_c <- ggplot(dac_data%>% filter(dac!= "total"), aes(x = hispanic_prop, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(name = "Screening Tools", values = palette)+
  geom_density(data = dac_data%>% filter(dac== "total"), aes(x = hispanic_prop), color = "black")+
  ylab("") +
  xlab("Proportion of Hispanic/Latino\nResidents")
part_d <- ggplot(dac_data %>% filter(dac!= "total"), aes(x = black_prop, group = dac, color = dac)) +
  geom_density() +
  theme_classic() +
  scale_color_manual(name = "Screening Tools", values = palette)+
  geom_density(data = dac_data%>% filter(dac== "total"), aes(x = black_prop), color = "black")+
  ylab("") +
  xlab("Proportion of Black\nResidents")

# Create legend
legend <- get_legend (ggplot(dac_data  %>% filter(dac!= "total"), aes(x = black_prop, y = hispanic_prop, group = dac, color = dac)) +
  geom_line()+
  theme_classic() +
  scale_color_manual(name = "Screening Tools", values = palette, labels = c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  labs(color = "Tool")+
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom"))
  
# Arrange figure parts and export ---------------------------
figure_s1_plots <- cowplot::plot_grid(part_a + theme(legend.position="none"), 
                                part_b+ theme(legend.position="none"),
                                part_c+ theme(legend.position="none"),
                                part_d+ theme(legend.position="none"),
                               nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")
# add common legend
figure_s1 <- cowplot::plot_grid(figure_s1_plots, legend, nrow = 2, rel_heights = c(1, .1))

cowplot::ggsave2("outputs/figures/fig_s1.png", figure_s1,
                 width = 7,
                 height = 6,
                 units = c("in"))

