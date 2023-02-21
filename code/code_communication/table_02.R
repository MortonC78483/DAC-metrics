# ---------------------------
##
## Script name: table_02
##
## Purpose of script:
##    Create table 2, which shows the number of people who moved from being in a DAC to not being in a DAC
##    for all metrics.
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
library(stargazer) # for table exporting

# Functions ---------------------------
# function to append proportion to vector of numbers
add_perc <- function(data, denom) {
  return(paste0(data, " (", round(data/denom, 3), ")"))
}

# function to create table elements
# we will filter data to places that have 0 people in DACs in metric 2, 
# and then sum metric 1 to get people in DACs in metric 1 that are no longer
# in DACs in metric 2
get_n_diff <- function(data, metric_1, metric_2){
  return(sum(data[data[metric_2] == 0,][metric_1], na.rm = T))
}

# function to calculate how many people are classified as disadvantaged
# by both metrics
get_n_same <- function(data, metric_1, metric_2){
  return(sum(data[data[metric_2] > 0,][metric_1], na.rm = T))
}

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_block_group.csv") %>%
  dplyr::select(total_race_eth, 
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac) %>%
  as.data.frame %>%
  mutate(
    across(-total_race_eth, ~ .*total_race_eth)
  )

metrics <- c("univariate_dac", "ces_dac", "ces_dac_adj", "eji_dac", "cejst_dac")

# Create table ---------------------------
table_02 <- data.frame(matrix(ncol = 5, nrow = 5),
  row.names = metrics)
colnames(table_02) = metrics

for (metric_1 in metrics) {
  for (metric_2 in metrics) {
    table_02[metric_1, metric_2] = get_n_diff(data, metric_1, metric_2)
  }
}

table_02 <- data.frame(lapply(table_02, add_perc, (sum(data$total_race_eth))))

rownames(table_02) = metrics

# Save table ---------------------------
stargazer(table_02, 
          summary=FALSE, rownames=TRUE,
          type = "html", out = "outputs/tables/table_02.html")

# Create figure version of table ---------------------------
fig_data <- data.frame(matrix(ncol = 4, nrow = 10))
colnames(fig_data) = c('metrics', 'metric1only','both','metric2only')

row = 1;
for (i in 1:(length(metrics)-1)) {
  for (j in (i+1):length(metrics)) {
    fig_data[row,] = c(paste0(metrics[i], metrics[j]), 
                      get_n_diff(data, metrics[i], metrics[j]),
                      get_n_same(data, metrics[i], metrics[j]),
                      get_n_diff(data, metrics[j], metrics[i]))
    row = row + 1
  }
}


fig_data <- data.frame(matrix(ncol = 3, nrow = 10*3))
colnames(fig_data) = c('metrics', 'type','val')

row = 1;
for (i in 1:(length(metrics)-1)) {
  for (j in (i+1):length(metrics)) {
    fig_data[row,] = c(paste0(metrics[i], '\n', metrics[j]), 'metric_1', as.numeric(get_n_diff(data, metrics[i], metrics[j])))
    row = row + 1
    fig_data[row,] = c(paste0(metrics[i], '\n',metrics[j]), 'both', as.numeric(get_n_same(data, metrics[i], metrics[j])))
    row = row + 1
    fig_data[row,] = c(paste0(metrics[i], '\n',metrics[j]), 'metric_2', as.numeric(get_n_diff(data, metrics[j], metrics[i])))
    row = row + 1
  }
}


ggplot(fig_data,                         # Draw barplot with grouping & stacking
       aes(x = metrics,
           y = as.numeric(val),
           fill = factor(type, 
                         level = c('metric_1', 'both', 'metric_2')))) + 
  geom_bar(stat = "identity",
           position = "stack")+
  scale_fill_manual(values=c("#ff4f72", "#984fff", "#4f92ff"))+
  xlab('')+
  ylab('Number of People')+
  theme_classic()+
  theme(legend.position='none')


