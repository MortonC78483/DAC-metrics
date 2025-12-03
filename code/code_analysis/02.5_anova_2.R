# ## ---------------------------
# ##
# ## Script name: 02_anova
# ##
# ## Purpose of script:
# ##    This script determines, for block groups, whether DAC classifications from metrics (CES, CES
# ##    adjusted, CEJST, EJI, and New Jersey-like) result in different mean population density, hazards,
# ##    and below 2x federal poverty level. Run post-hoc Tukey test for significant ANOVA results
# ##
# ## Author: Claire Morton
# ##
# ## Date Created: 2022-12-07
# ##
# ## Email: mortonc@stanford.edu
# ##
# ## ---------------------------
# ##
# ## Notes:
# ##   
# ##
# ## ---------------------------
# 
# # Install packages ---------------------------
# library(readr)
# library(tidyverse)
# library(reshape2)
# 
# # Import data ---------------------------
# data <- read_csv("data/data_processed/metrics_tract.csv") %>%
#   dplyr::select(total_race_eth, hispanic, Black, pop_density, over_200_percent_poverty, total_poverty,
#                 univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
#   mutate(pov = total_poverty-over_200_percent_poverty)
# 
# # Format data ---------------------------
# # Create dataset with DAC designation type and variables of interest
# univariate_dac_data <- data %>%
#   dplyr::filter(univariate_dac == 1) %>%
#   mutate(dac = "univariate")
# ces_dac_data <- data %>%
#   dplyr::filter(ces_dac == 1) %>%
#   mutate(dac = "ces")
# ces_dac_adj_data <- data %>%
#   dplyr::filter(ces_dac_adj == 1) %>%
#   mutate(dac = "ces_adj")
# eji_dac_data <- data %>%
#   dplyr::filter(eji_dac == 1) %>%
#   mutate(dac = "eji")
# cejst_dac_data <- data %>%
#   dplyr::filter(cejst_dac == 1) %>%
#   mutate(dac = "cejst")
# 
# dac_data <- rbind(univariate_dac_data, ces_dac_data, ces_dac_adj_data, eji_dac_data, cejst_dac_data)
# 
# dac_data <- dac_data %>%
#   group_by(dac) %>% # group by tool of interest
#   mutate(Black_prop = ((sum(Black))/sum(total_race_eth)) / (sum(data$Black)/sum(data$total_race_eth)),
#          hisp_prop = ((sum(hispanic))/sum(total_race_eth)) / (sum(data$hispanic)/sum(data$total_race_eth)),
#          pov_prop = ((sum(pov))/sum(total_poverty)) / (sum(data$pov)/sum(data$total_poverty))) %>%
#   dplyr::select(c(dac, Black_prop, hisp_prop, pov_prop)) %>%
#   unique() %>%
#   melt(id = "dac")
# 
# # Make plot ---------------------------
# ggplot(data = dac_data, aes(x = variable, y = value, group = dac, color = dac)) +
#   geom_point(size = 3) +
#   xlab("")+
#   ylab("Proportion of group classified as living in DACs by tool/\nProportion of group in state")+
#   scale_x_discrete(labels = c("Black", "Hispanic", "Poverty")) +
#   ylim(1, max(dac_data$value))+
#   geom_hline(yintercept = 1, linetype = "dashed")+
#   theme_classic()
# 
# 
# 
