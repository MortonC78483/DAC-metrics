## ---------------------------
##
## Script name: figure_02
##
## Purpose of script:
##    Create figure 2, ANOVA results with significance indicators for five metrics
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
  dplyr::select(total_race_eth, hispanic, nonhispanic_black, pop_density, over_200_percent_poverty, total_poverty,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty,
         hispanic_prop = hispanic/total_race_eth,
         nh_black_prop = nonhispanic_black/total_race_eth,
         pop_density = pop_density * 1000000) %>% # people/square kilometer
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic, nonhispanic_black))

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

# plot
part_a <- ggplot(aes(y = pop_density, x = factor(dac)), data = dac_data)+
  xlab("Metric")+
  ylab("Population Density (People/Square Kilometer)")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max)+
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

# plot
part_b <- ggplot(aes(y = poverty_prop, x = factor(dac)), data = dac_data)+
  xlab("Metric")+
  ylab("Proportion People in Poverty")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max)+
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

# plot
part_c <- ggplot(aes(y = hispanic_prop, x = factor(dac)), data = dac_data)+
  xlab("Metric")+
  ylab("Proportion Hispanic/Latino People")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max)+
  stat_pvalue_manual(tukey.hsd, hide.ns = TRUE, step.increase = .02, tip.length = 0.01)+
  theme_classic()
#geom_jitter(position=position_jitter(width=.2), size=3) 

# Part d ---------------------------
one.way <- dac_data %>% anova_test(nh_black_prop ~ dac)
tukey.hsd <- dac_data %>% 
  tukey_hsd(nh_black_prop ~ dac) %>% 
  add_xy_position(x = "dac", 
                  fun = "mean_sd",
                  step.increase = 0.2)

# plot
part_d <- ggplot(aes(y = nh_black_prop, x = factor(dac)), data = dac_data)+
  xlab("Metric")+
  ylab("Proportion Non-Hispanic Black People")+
  scale_x_discrete(labels=c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  stat_summary(fun.data = min.mean.sd.max)+
  stat_pvalue_manual(tukey.hsd, hide.ns = TRUE, step.increase = .02, tip.length = 0.01)+
  theme_classic()

# Arrange figure parts and export ---------------------------
figure_2 <- cowplot::plot_grid(part_a, part_b, part_c, part_d,
                               nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")
cowplot::ggsave2("outputs/figures/fig_2.png", figure_2,
                 width = 13,
                 height = 9,
                 units = c("in"))


# Drafts/deprecated ---------------------------
# # Visualization draft: box plots with p-values
# one.way <- dac_data %>% anova_test(pop_density ~ dac)
# tukey.hsd <- dac_data %>% tukey_hsd(pop_density ~ dac)
#
# tukey.hsd <- tukey.hsd %>% add_xy_position(x = "dac", 
#                                            step.increase = 0.2)
# ggboxplot(dac_data, x = "dac", y = "pop_density") +
#   stat_pvalue_manual(tukey.hsd, hide.ns = T, step.increase = .025) +
#   labs(
#     subtitle = get_test_label(one.way, detailed = TRUE),
#     caption = get_pwc_label(tukey.hsd)
#   )
