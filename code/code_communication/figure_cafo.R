## ---------------------------
##
## Script name: figure_05
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
#library(dataframe)
library(ggpubr)
library(cowplot)

# Import data ---------------------------
toplot_cafo <- read_csv("data/data_processed/hypergeometric_cafo.csv")

# Colors ---------------------------
palette <- RColorBrewer::brewer.pal(5, "Dark2")
palette[1] = "#2caadb"

# Make plots ---------------------------
plot<-ggplot(toplot_cafo, aes(x = as.factor(tool), y = val, group = as.factor(tool), color = as.factor(tool)))+
  geom_point()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  scale_color_manual(name = "Screening Tools", values = palette)+
  ylab("Odds Ratios for CAFO Exposure")+
  xlab("Tool")+
  scale_x_discrete(labels = c("CEJST", "CES", "CES+", "EJI", "Trivariate"))+
  theme_classic()+
  theme(legend.position = "none")+
  ylim(0, max(toplot_cafo$upper))+
  geom_hline(yintercept = 1, linetype = "dashed")

plot

cowplot::ggsave2("outputs/figures/fig_cafo_priority.png", plot,
                 width = 5,
                 height = 5,
                 units = c("in"))


