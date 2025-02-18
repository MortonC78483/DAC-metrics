## ---------------------------
##
## Script name: figure_06
##
## Purpose of script:
##    This script plots results of the analysis with Wallenius' Hypergeometric distribution
##
## Author: Claire Morton
##
## Date Created: 2023-05-19
##
## Email: mortonc@stanford.edu
##
## ---------------------------
##
##
## ---------------------------

# Install packages ---------------------------
library(readr)
library(tidyverse)
library(dataframe)
library(ggpubr)
library(cowplot)

# Import data ---------------------------
toplot_a <- read_csv("data/data_processed/hypergeometric_a.csv")
toplot_b <- read_csv("data/data_processed/hypergeometric_b.csv")

# Colors ---------------------------
palette <- RColorBrewer::brewer.pal(5, "Dark2")
palette[1] = "#2caadb"

# Make plots ---------------------------
a<-ggplot(toplot_a, aes(x = as.factor(tool), y = val, group = as.factor(tool), color = as.factor(tool)))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  scale_color_manual(name = "Screening Tools", values = palette)+
  ylab("Odds Ratios for Proportion Black People")+
  xlab("Tool")+
  scale_x_discrete(labels = c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  theme_classic()+
  theme(legend.position = "none")+
  ylim(0, max(toplot_a$upper))+
  geom_hline(yintercept = 1, linetype = "dashed")

b<- ggplot(toplot_b, aes(x = as.factor(tool), y = val, group = as.factor(tool), color = as.factor(tool)))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  scale_color_manual(name = "Screening Tools", values = palette)+
  ylab("Odds Ratios for Proportion Hispanic/Latino People")+
  xlab("Tool")+
  scale_x_discrete(labels = c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  theme_classic()+
  theme(legend.position = "none")+
  ylim(0, max(toplot_b$upper))+
  geom_hline(yintercept = 1, linetype = "dashed")

# Presentation figure (dark background)
# ggplot(toplot_b, aes(x = as.factor(tool), y = val, group = as.factor(tool), color = as.factor(tool)))+
#   geom_point()+
#   geom_errorbar(aes(ymin = lower, ymax = upper))+
#   scale_color_brewer(palette = "Set2")+
#   ylab("Estimated Weights for Proportion Hispanic/Latino People")+
#   xlab("Tool")+
#   scale_x_discrete(labels = c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
#   theme_classic()+
#   dark_theme_classic()+
#   theme(legend.position = "none")

figure_6 <- cowplot::plot_grid(a, b, 
                               nrow = 1, axis = "tblr", align = "hv", labels = "AUTO")

figure_6

cowplot::ggsave2("outputs/figures/fig_6.png", figure_6,
                 width = 8,
                 height = 5,
                 units = c("in"))


