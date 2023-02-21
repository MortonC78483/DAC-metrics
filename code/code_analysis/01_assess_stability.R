## ---------------------------
##
## Script name: 01_compare_ces
##
## Purpose of script:
##    This script determines, for census tracts, whether rankings from CES and CES adjusted are 
##    significantly different by running a Wilcoxon signed rank test on tract rankings
##
## Author: Claire Morton
##
## Date Created: 2022-12-07
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
library(MASS)

# Import data ---------------------------
ces <- read_csv("data/data_interim/recalculated_ces_scores.csv")

# Run test ---------------------------
wilcox.test(ces$ces_score, ces$ces_score_adj, paired=TRUE) 

# Calculate Kendall's Tau ---------------------------
cor.test(ces$ces_perc, ces$ces_perc_adj, method="kendall")

# obtain distance ---------------------------
# average change in score
mean(abs(ces$ces_score - ces$ces_score_adj), na.rm = T)

# average change in percentile
mean(abs(ces$ces_perc - ces$ces_perc_adj), na.rm = T)

# number of tracts that become disadvantaged
ces %>%
  filter(ces_perc_adj >= 75) %>%
  filter(ces_perc < 75) %>%
  nrow()



