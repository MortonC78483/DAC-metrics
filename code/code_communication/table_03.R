## ---------------------------
##
## Script name: table_03
##
## Purpose of script:
##    Calculate table of Pearson's correlation between the metrics
##
## Author: Claire Morton
##
## Date Created: 2022-12-31
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
library(tigris)
library(sf)
library(stargazer)
library(psych)
# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  dplyr::select(GEOID, univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)

# Create table ---------------------------
metric_names = c("univariate_dac", "ces_dac", "ces_dac_adj", "eji_dac", "cejst_dac")
data_frame = data.frame(matrix(ncol = 3, nrow = 0))
colnames(data_frame) = c("metric1", "metric2", "cor")
for (i in 1:(length(metric_names)-1)) {
  for (j in (i+1):length(metric_names)) {
    metric_one = metric_names[i]
    metric_two = metric_names[j]
    data_frame[nrow(data_frame) + 1,] = c(metric_names[i], metric_names[j], round(cor(data[metric_one], data[metric_two], method = "pearson", use = "complete.obs"), 3))
    }
}
data_frame

# Format table for export ---------------------------
table <- matrix(nrow = length(metric_names), ncol = length(metric_names), dimnames = list(metric_names, metric_names))
for (i in 1:(length(metric_names)-1)){ # row
  for (j  in (i+1):length(metric_names)){ # column
    val = round(as.numeric((data_frame %>%
             filter(data_frame$metric1 == metric_names[i] & data_frame$metric2 == metric_names[j]))$cor), 3)
    table[i, j] = val
  }
}

write.csv(table, "data/data_interim/table_03.csv")
# Export table ---------------------------
names = c("Trivariate", "CES", "CES+", "EJI", "CEJST")
rownames = names

colnames(table) = names
table <- round(table, 2)
rownames(table) = rownames
stargazer(table, 
          summary=FALSE, rownames=TRUE,
          type = "html", out = "outputs/tables/table_03.html")

