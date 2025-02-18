## ---------------------------
##
## Script name: figure_03
##
## Purpose of script:
##    Create figure 3, ANOVA results with significance indicators for five metrics
##
## Author: Claire Morton
##
## Date Created: 2022-12-30
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
library(rstatix)
library(cowplot)

# function for mean +- standard deviation
min.mean.sd.max <- function(x) {
  r <- c(mean(x) - sd(x), mean(x), mean(x) + sd(x))
  names(r) <- c("ymin", "y", "ymax")
  r
}

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(total_race_eth, hispanic, Black, pop_density, over_200_percent_poverty, total_poverty,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty,
         hispanic_prop = hispanic/total_race_eth,
         black_prop = Black/total_race_eth,
         pop_density = pop_density * 1000000) %>% # people/square kilometer
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic, Black))

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

dac_data <- rbind(univariate_dac_data, ces_dac_data, ces_dac_adj_data, eji_dac_data, cejst_dac_data)


# Part a ---------------------------
one.way <- dac_data %>% anova_test(pop_density ~ dac) #people/square km
tukey.hsd <- dac_data %>% 
  tukey_hsd(pop_density ~ dac) %>% 
  add_xy_position(x = "dac", 
                  fun = "mean_sd",
                  step.increase = 0.2)
mag_a <- max(abs(tukey.hsd$estimate)) # get max magnitude difference between means

# plot
part_a <- ggplot(aes(y = pop_density, x = factor(dac)), data = dac_data)+
  xlab("Screening Tool")+
  ylab("Population Density\n(People/Square Kilometer)")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max, color = 'black')+
  stat_pvalue_manual(tukey.hsd, 
                     hide.ns = TRUE, 
                     step.increase = .01, 
                     tip.length = 0.01)+
  theme_classic()
#geom_jitter(position=position_jitter(width=.2), size=3) 

# Part b ---------------------------
one.way <- dac_data %>% anova_test(poverty_prop ~ dac)
tukey.hsd <- dac_data %>% 
  tukey_hsd(poverty_prop ~ dac) %>% 
  add_xy_position(x = "dac", 
                  fun = "mean_sd",
                  step.increase = 0.2)
mag_b <- max(abs(tukey.hsd$estimate)) # get max magnitude difference between means

# plot
part_b <- ggplot(aes(y = poverty_prop, x = factor(dac)), data = dac_data)+
  xlab("Screening Tool")+
  ylab("Proportion People in Poverty")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max, color = 'black')+
  stat_pvalue_manual(tukey.hsd, 
                     hide.ns = TRUE, 
                     step.increase = .01, 
                     tip.length = 0.01)+
  theme_classic()
#geom_jitter(position=position_jitter(width=.2), size=3) 

# Part c ---------------------------
one.way <- dac_data %>% anova_test(hispanic_prop ~ dac)
tukey.hsd <- dac_data %>% 
  tukey_hsd(hispanic_prop ~ dac) %>% 
  add_xy_position(x = "dac", 
                  fun = "mean_sd",
                  step.increase = 0.2)
mag_c <- max(abs(tukey.hsd$estimate)) # get max magnitude difference between means

# plot
part_c <- ggplot(aes(y = hispanic_prop, x = factor(dac)), data = dac_data)+
  xlab("Screening Tool")+
  ylab("Proportion Hispanic/Latino People")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max, color = 'black')+
  stat_pvalue_manual(tukey.hsd, hide.ns = TRUE, step.increase = .02, tip.length = 0.01)+
  theme_classic()+
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1))
#geom_jitter(position=position_jitter(width=.2), size=3) 

# Part d ---------------------------
one.way <- dac_data %>% anova_test(black_prop ~ dac)
tukey.hsd <- dac_data %>% 
  tukey_hsd(black_prop ~ dac) %>% 
  add_xy_position(x = "dac", 
                  fun = "mean_sd",
                  step.increase = 0.2)
mag_d <- max(abs(tukey.hsd$estimate)) # get max magnitude difference between means


# plot
part_d <- ggplot(aes(y = black_prop, x = factor(dac)), data = dac_data)+
  xlab("Screening Tool")+
  ylab("Proportion Black People")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max, color = 'black')+
  stat_pvalue_manual(tukey.hsd, hide.ns = TRUE, step.increase = .02, tip.length = 0.01)+
  theme_classic()

# Arrange figure parts and export ---------------------------
figure_3 <- cowplot::plot_grid(part_a, part_b, part_c, part_d,
                               nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")

cowplot::ggsave2("outputs/figures/fig_3.png", figure_3,
                 width = 8,
                 height = 6,
                 units = c("in"))

# Get widest range of differences between means 
mag_a
mag_b
mag_c
mag_d