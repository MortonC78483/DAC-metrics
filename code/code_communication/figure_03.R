## ---------------------------
##
## Script name: figure_03
##
## Purpose of script:
##    Create figure 3, likelihood of living in a DAC based on population 
##    density, proportion people in poverty, proportion Hispanic/Latino people
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
##    Figures similar to this national-level analysis:
##    https://grist.org/equity/climate-and-economic-justice-screening-tool-race/ 
##
## ---------------------------

# Install packages ---------------------------
library(readr)
library(tidyverse)
library(ggplot2) 
library(ggpubr)

var_percentile = "ces_dac"
var_to_plot = "poverty_prop"

create_plot <- function(data, var_percentile, var_to_plot, cut_points = seq(0, 100, 5), cut_labels = seq(5, 100, 5)) {
  data_to_plot <- data %>%
    select(var_percentile, var_to_plot)%>%
    mutate(dis = ifelse(!!as.symbol(var_percentile) == 1, 1, 0))
  data_to_plot <- data_to_plot %>%
    mutate(cut = cut(x = deframe(data_to_plot[,var_to_plot]), 
                     cut_points, 
                     include.lowest = TRUE,
                     labels = cut_labels)) %>%
    group_by(cut) %>%
    summarize(mean = mean(dis, na.rm = T), 
              n_dis = sum(dis, na.rm = T), 
              n_not_dis = n() - sum(dis, na.rm = T), 
              n = n()) %>%
    filter(!is.na(cut))
  
  plot_a = ggplot(data_to_plot) +
    geom_point(aes(x = cut, y = mean), stat = "identity") +
    xlab("")+
    ylab("Proportion of block groups in disadvantaged communities") +
    theme_classic()
  
  # plot_b = ggplot(data_to_plot) +
  #   geom_bar(aes(x = cut, y = n_dis), stat = "identity") +
  #   xlab("")+
  #   ylab("Number of tracts in top 25% of CES") +
  #   theme_classic()
  
  # ggarrange(plot_a, plot_b)
  return(data_to_plot %>% dplyr::select(cut, mean))
}

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(total_race_eth, hispanic, nonhispanic_black, pop_density, over_200_percent_poverty, total_poverty,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  mutate(poverty_prop = (total_poverty-over_200_percent_poverty)/total_poverty * 100,
         nh_black_prop = nonhispanic_black/total_race_eth,
         hispanic_prop = hispanic/total_race_eth * 100) %>%
  dplyr::select(-c(over_200_percent_poverty, total_poverty, total_race_eth, hispanic, nonhispanic_black))

# Create part a ---------------------------
cut_points = c(quantile(data$pop_density, probs = seq(0, 1, length.out = 21), na.rm = T))
  
  # cut_points = seq(0, max(data$pop_density, na.rm = T), length.out = 21)
ces_pov <- create_plot(data, "ces_dac", "pop_density", cut_points = cut_points) %>%
  mutate(metric = "CES")
ces_adj_pov <- create_plot(data, "ces_dac_adj", "pop_density", cut_points = cut_points) %>%
  mutate(metric = "CES+")
eji_pov <- create_plot(data, "eji_dac", "pop_density", cut_points = cut_points) %>%
  mutate(metric = "EJI")
cejst_pov <- create_plot(data, "cejst_dac", "pop_density", cut_points = cut_points) %>%
  mutate(metric = "CEJST")
univariate_pov <- create_plot(data, "univariate_dac", "pop_density", cut_points = cut_points) %>%
  mutate(metric = "Trivariate")
density_data <- rbind(ces_pov, ces_adj_pov, eji_pov, cejst_pov, univariate_pov)
part_a <- ggplot(data = density_data, aes(x = cut, y = mean, group = metric, color = metric))+
  geom_point()+
  theme_classic()+
  ylab("Proportion of block groups\nin Disadvantaged Communities")+
  xlab("Population Density (Percentiles)")+
  ylim(0, 1)

# Create part b ---------------------------
cut_points = c(quantile(data$poverty_prop, probs = seq(0, 1, length.out = 21), na.rm = T))

ces_pov <- create_plot(data, "ces_dac", "poverty_prop", cut_points = cut_points) %>%
  mutate(metric = "CES")
ces_adj_pov <- create_plot(data, "ces_dac_adj", "poverty_prop", cut_points = cut_points) %>%
  mutate(metric = "CES+")
eji_pov <- create_plot(data, "eji_dac", "poverty_prop", cut_points = cut_points) %>%
  mutate(metric = "EJI")
cejst_pov <- create_plot(data, "cejst_dac", "poverty_prop", cut_points = cut_points) %>%
  mutate(metric = "CEJST")
univariate_pov <- create_plot(data, "univariate_dac", "poverty_prop", cut_points = cut_points) %>%
  mutate(metric = "Trivariate")
pov_data <- rbind(ces_pov, ces_adj_pov, eji_pov, cejst_pov, univariate_pov)
part_b <- ggplot(data = pov_data, aes(x = cut, y = mean, group = metric, color = metric))+
  geom_point()+
  theme_classic()+
  ylab("Proportion of block groups\nin Disadvantaged Communities")+
  xlab("Percentage of Households in Poverty (Percentiles)")+
  ylim(0, 1)

# Create part c ---------------------------
cut_points = c(quantile(data$hispanic_prop, probs = seq(0, 1, length.out = 21), na.rm = T))

# cut_points = seq(0, max(data$pop_density, na.rm = T), length.out = 21)
ces_pov <- create_plot(data, "ces_dac", "hispanic_prop", cut_points = cut_points) %>%
  mutate(metric = "CES")
ces_adj_pov <- create_plot(data, "ces_dac_adj", "hispanic_prop", cut_points = cut_points) %>%
  mutate(metric = "CES+")
eji_pov <- create_plot(data, "eji_dac", "hispanic_prop", cut_points = cut_points) %>%
  mutate(metric = "EJI")
cejst_pov <- create_plot(data, "cejst_dac", "hispanic_prop", cut_points = cut_points) %>%
  mutate(metric = "CEJST")
univariate_pov <- create_plot(data, "univariate_dac", "hispanic_prop", cut_points = cut_points) %>%
  mutate(metric = "Trivariate")
density_data <- rbind(ces_pov, ces_adj_pov, eji_pov, cejst_pov, univariate_pov)
part_c <- ggplot(data = density_data, aes(x = cut, y = mean, group = metric, color = metric))+
  geom_point()+
  theme_classic()+
  ylab("Proportion of block groups\nin Disadvantaged Communities")+
  xlab("Proportion Hispanic/Latino Residents (Percentiles)")+
  ylim(0, 1)

# Create part d ---------------------------
cut_points = c(quantile(data$nh_black_prop, probs = seq(0, 1, length.out = 21), na.rm = T))
cut_points = cut_points + seq_along(cut_points) * .Machine$double.eps

# cut_points = seq(0, max(data$pop_density, na.rm = T), length.out = 21)
ces_pov <- create_plot(data, "ces_dac", "nh_black_prop", cut_points = cut_points) %>%
  mutate(metric = "CES")
ces_adj_pov <- create_plot(data, "ces_dac_adj", "nh_black_prop", cut_points = cut_points) %>%
  mutate(metric = "CES+")
eji_pov <- create_plot(data, "eji_dac", "nh_black_prop", cut_points = cut_points) %>%
  mutate(metric = "EJI")
cejst_pov <- create_plot(data, "cejst_dac", "nh_black_prop", cut_points = cut_points) %>%
  mutate(metric = "CEJST")
univariate_pov <- create_plot(data, "univariate_dac", "nh_black_prop", cut_points = cut_points) %>%
  mutate(metric = "Trivariate")
density_data <- rbind(ces_pov, ces_adj_pov, eji_pov, cejst_pov, univariate_pov)
part_d <- ggplot(data = density_data, aes(x = cut, y = mean, group = metric, color = metric))+
  geom_point()+
  theme_classic()+
  ylab("Proportion of block groups\nin Disadvantaged Communities")+
  xlab("Proportion Non-Hispanic Black Residents (Percentiles)")+
  ylim(0, 1)


# Arrange figure parts and export ---------------------------
figure_3 <- cowplot::plot_grid(part_a, part_b, part_c, part_d,
                               nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")
cowplot::ggsave2("outputs/figures/fig_3.png", figure_3,
                 width = 13,
                 height = 9,
                 units = c("in"))

# Histograms of distributions on x axes (extra) ---------------------------
# part_a <- ggplot(data, aes(x = pop_density*1000000)) + # people/square km
#   geom_density()+
#   ylab("Density")+
#   xlab("Population Density (People/km^2)")+
#   theme_classic()
# part_b <- ggplot(data, aes(x = poverty_prop))+
#   geom_density()+
#   ylab("Density")+
#   xlab("Percentage of Households in Poverty")+
#   theme_classic()
# part_c <- ggplot(data, aes(x = hispanic_prop))+
#   geom_density()+
#   ylab("Density")+
#   xlab("Proportion Hispanic/Latino Residents")+
#   theme_classic()
# part_d <- ggplot(data, aes(x = nh_black_prop))+
#   geom_density()+
#   ylab("Density")+
#   xlab("Proportion Non-Hispanic Black Residents")+
#   theme_classic()
# cowplot::plot_grid(part_a, part_b, part_c, part_d,
#                    nrow = 2, axis = "tblr", align = "hv", labels = "AUTO")
