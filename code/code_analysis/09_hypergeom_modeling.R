## ---------------------------
##
## Script name: 09_hypergeom_modeling
##
## Purpose of script:
##    This script will assess whether using Wallenius's noncentral hypergeometric distribution is
##    a good way to think about comparing the tools to classify disadvantaged communities.
##
## Author: Claire Morton
##
## Date Created: 2023-05-03
##
## Email: mortonc@stanford.edu
##
## ---------------------------
##
## Notes:
##  Formulas used for Wallenius' hypergeometric distribution:
##  https://en.wikipedia.org/wiki/Wallenius%27_noncentral_hypergeometric_distribution 
##
## ---------------------------

# Install packages ---------------------------
library(readr)
library(tidyverse)
library(dataframe)
library(ggpubr)

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") 

med_black_prop <- median(data$Black/data$total_race_eth, na.rm = T)
med_hisp_prop <- median(data$hispanic/data$total_race_eth, na.rm = T)

above_med_black <- data %>%
  filter(data$Black/data$total_race_eth >= med_black_prop)
above_med_black_ces <- sum(above_med_black$ces_dac, na.rm = T)         
above_med_black_ces_adj <- sum(above_med_black$ces_dac_adj, na.rm = T)         
above_med_black_eji <- sum(above_med_black$eji_dac, na.rm = T)         
above_med_black_cejst <- sum(above_med_black$cejst_dac, na.rm = T)         
above_med_black_trivariate <- sum(above_med_black$univariate_dac, na.rm = T)    

above_med_hisp <- data %>%
  filter(data$hispanic/data$total_race_eth >= med_hisp_prop)
above_med_hisp_ces <- sum(above_med_hisp$ces_dac, na.rm = T)         
above_med_hisp_ces_adj <- sum(above_med_hisp$ces_dac_adj, na.rm = T)         
above_med_hisp_eji <- sum(above_med_hisp$eji_dac, na.rm = T)         
above_med_hisp_cejst <- sum(above_med_hisp$cejst_dac, na.rm = T)         
above_med_hisp_trivariate <- sum(above_med_hisp$univariate_dac, na.rm = T)         

# Now, our "urn" (California), contains nrow(above_med_x) block groups above median
# and nrow(data)-nrow(above_med_x) block groups below median, so these are our m1 and m2
# We know our observed number of block groups above median by metric, so we substitute these 
# in as our mu. We know how many trials we got, n, because this is the sum(data$x_dac == 1). 
# We solve for weight. Higher weight indicates that, when we model the process of assigning
# block groups to classifications using a distribution that controls for sampling without 
# replacement, one metric is predicted to prioritize block groups above the statewide median
# for the characteristic more than another.

# Solves for weight in Wallenius's hypergeometric distribution, given other parameters
solve_wallenius <- function(m1, m2, mu, n){
  num = log(1-(mu/m1))
  denom = log(1-((n-mu)/m2))
  return(num/denom)
}

# calculate weights for all metrics
# proportion Black
m1 = nrow(above_med_black)
m2 = nrow(data)-nrow(above_med_black)
solve_wallenius(m1, m2, above_med_black_ces, sum(data$ces_dac == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_black_ces_adj, sum(data$ces_dac_adj == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_black_cejst, sum(data$cejst_dac == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_black_eji, sum(data$eji_dac == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_black_trivariate, sum(data$univariate_dac == 1, na.rm = T))

# proportion Hispanic
m1 = nrow(above_med_hisp)
m2 = nrow(data)-nrow(above_med_hisp)
solve_wallenius(m1, m2, above_med_hisp_ces, sum(data$ces_dac == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_hisp_ces_adj, sum(data$ces_dac_adj == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_hisp_cejst, sum(data$cejst_dac == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_hisp_eji, sum(data$eji_dac == 1, na.rm = T))
solve_wallenius(m1, m2, above_med_hisp_trivariate, sum(data$univariate_dac == 1, na.rm = T))

## Putting Confidence Intervals on Estimated Weights --------------------------------------------
# Bootstrapping --------------------------------------------
# I'm going to approximate a confidence interval with bootstrapping. For a given tool, I'll
# sample with replacement from the original data to get a boostrap sample 
# (I'll take M boostrap samples). Then I'll recalculate all of the above measurements to get a 
# bootstrapped weight estimate.
# When we have M bootstrap weights, we will calculate a 95% confidence interval by computing 
# [w - q(1-alpha/2)(w*-w), w - q(alpha/2)(w*-w)] where q(x)(w*-w) represents the xth quantile
# of the distribution of boostrap w's - original w estimate

set.seed(NULL)
set.seed(100)
M = 250
w_ces_black = c()
w_ces_adj_black = c()
w_cejst_black = c()
w_eji_black = c()
w_trivariate_black = c()
w_ces_hisp = c()
w_ces_adj_hisp = c()
w_cejst_hisp = c()
w_eji_hisp = c()
w_trivariate_hisp = c()
for (i in c(1:M)){
  # create a boostrap sample
  sample <- data[sample(nrow(data), nrow(data), replace = T),]
  med_black_prop <- median(sample$Black/sample$total_race_eth, na.rm = T)
  med_hisp_prop <- median(sample$hispanic/sample$total_race_eth, na.rm = T)
  
  above_med_black <- sample %>%
    filter(sample$Black/sample$total_race_eth >= med_black_prop)
  above_med_black_ces <- sum(above_med_black$ces_dac, na.rm = T)         
  above_med_black_ces_adj <- sum(above_med_black$ces_dac_adj, na.rm = T)         
  above_med_black_eji <- sum(above_med_black$eji_dac, na.rm = T)         
  above_med_black_cejst <- sum(above_med_black$cejst_dac, na.rm = T)         
  above_med_black_trivariate <- sum(above_med_black$univariate_dac, na.rm = T)    
  
  above_med_hisp <- sample %>%
    filter(sample$hispanic/sample$total_race_eth >= med_hisp_prop)
  above_med_hisp_ces <- sum(above_med_hisp$ces_dac, na.rm = T)         
  above_med_hisp_ces_adj <- sum(above_med_hisp$ces_dac_adj, na.rm = T)         
  above_med_hisp_eji <- sum(above_med_hisp$eji_dac, na.rm = T)         
  above_med_hisp_cejst <- sum(above_med_hisp$cejst_dac, na.rm = T)         
  above_med_hisp_trivariate <- sum(above_med_hisp$univariate_dac, na.rm = T)   
  
  # calculate weights for all metrics
  # proportion Black
  m1 = nrow(above_med_black)
  m2 = nrow(sample)-nrow(above_med_black)
  w_ces_black = append(w_ces_black, c(solve_wallenius(m1, m2, above_med_black_ces, sum(sample$ces_dac == 1, na.rm = T))))
  w_ces_adj_black = append(w_ces_adj_black, c(solve_wallenius(m1, m2, above_med_black_ces_adj, sum(sample$ces_dac_adj == 1, na.rm = T))))
  w_cejst_black = append(w_cejst_black, c(solve_wallenius(m1, m2, above_med_black_cejst, sum(sample$cejst_dac == 1, na.rm = T))))
  w_eji_black = append(w_eji_black, c(solve_wallenius(m1, m2, above_med_black_eji, sum(sample$eji_dac == 1, na.rm = T))))
  w_trivariate_black = append(w_trivariate_black, c(solve_wallenius(m1, m2, above_med_black_trivariate, sum(sample$univariate_dac == 1, na.rm = T))))

  # proportion Hispanic
  m1 = nrow(above_med_hisp)
  m2 = nrow(sample)-nrow(above_med_hisp)
  w_ces_hisp = append(w_ces_hisp, c(solve_wallenius(m1, m2, above_med_hisp_ces, sum(sample$ces_dac == 1, na.rm = T))))
  w_ces_adj_hisp = append(w_ces_adj_hisp, c(solve_wallenius(m1, m2, above_med_hisp_ces_adj, sum(sample$ces_dac_adj == 1, na.rm = T))))
  w_cejst_hisp = append(w_cejst_hisp, c(solve_wallenius(m1, m2, above_med_hisp_cejst, sum(sample$cejst_dac == 1, na.rm = T))))
  w_eji_hisp = append(w_eji_hisp, c(solve_wallenius(m1, m2, above_med_hisp_eji, sum(sample$eji_dac == 1, na.rm = T))))
  w_trivariate_hisp = append(w_trivariate_hisp, c(solve_wallenius(m1, m2, above_med_hisp_trivariate, sum(sample$univariate_dac == 1, na.rm = T))))
}

# Calculation of intervals --------------------------------------------
med_black_prop <- median(data$Black/data$total_race_eth, na.rm = T)
med_hisp_prop <- median(data$hispanic/data$total_race_eth, na.rm = T)

above_med_black <- data %>%
  filter(data$Black/data$total_race_eth >= med_black_prop)
above_med_black_ces <- sum(above_med_black$ces_dac, na.rm = T)         
above_med_black_ces_adj <- sum(above_med_black$ces_dac_adj, na.rm = T)         
above_med_black_eji <- sum(above_med_black$eji_dac, na.rm = T)         
above_med_black_cejst <- sum(above_med_black$cejst_dac, na.rm = T)         
above_med_black_trivariate <- sum(above_med_black$univariate_dac, na.rm = T)    

above_med_hisp <- data %>%
  filter(data$hispanic/data$total_race_eth >= med_hisp_prop)
above_med_hisp_ces <- sum(above_med_hisp$ces_dac, na.rm = T)         
above_med_hisp_ces_adj <- sum(above_med_hisp$ces_dac_adj, na.rm = T)         
above_med_hisp_eji <- sum(above_med_hisp$eji_dac, na.rm = T)         
above_med_hisp_cejst <- sum(above_med_hisp$cejst_dac, na.rm = T)         
above_med_hisp_trivariate <- sum(above_med_hisp$univariate_dac, na.rm = T)         

# Solves for weight in Wallenius's hypergeometric distribution, given other parameters
solve_wallenius <- function(m1, m2, mu, n){
  num = log(1-(mu/m1))
  denom = log(1-((n-mu)/m2))
  return(num/denom)
}

# When we have M bootstrap weights, we will calculate a 95% confidence interval by computing 
# [w - q(1-alpha/2)(w*-w), w - q(alpha/2)(w*-w)] where q(x)(w*-w) represents the xth quantile
# of the distribution of boostrap w's - original w estimate
# proportion Black
m1 = nrow(above_med_black)
m2 = nrow(data)-nrow(above_med_black)
alpha = .05

conf_ints_black <- c()
w = solve_wallenius(m1, m2, above_med_black_ces, sum(data$ces_dac == 1, na.rm = T))
conf_ints_black = cbind(conf_ints_black, "ces" = c(w - quantile(w_ces_black-w, 1-alpha/2), w, w - quantile(w_ces_black-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_black_ces_adj, sum(data$ces_dac_adj == 1, na.rm = T))
conf_ints_black = cbind(conf_ints_black, "ces_adj" = c(w - quantile(w_ces_adj_black-w, 1-alpha/2), w, w - quantile(w_ces_adj_black-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_black_cejst, sum(data$cejst_dac == 1, na.rm = T))
conf_ints_black = cbind(conf_ints_black, "cejst" = c(w - quantile(w_cejst_black-w, 1-alpha/2), w, w - quantile(w_cejst_black-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_black_eji, sum(data$eji_dac == 1, na.rm = T))
conf_ints_black = cbind(conf_ints_black, "eji" = c(w - quantile(w_eji_black-w, 1-alpha/2), w, w - quantile(w_eji_black-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_black_trivariate, sum(data$univariate_dac == 1, na.rm = T))
conf_ints_black = cbind(conf_ints_black, "trivariate" = c(w - quantile(w_trivariate_black-w, 1-alpha/2), w, w - quantile(w_trivariate_black-w, alpha/2)))

# proportion Hispanic
m1 = nrow(above_med_hisp)
m2 = nrow(data)-nrow(above_med_hisp)
alpha = .05

conf_ints_hisp <- c()
w = solve_wallenius(m1, m2, above_med_hisp_ces, sum(data$ces_dac == 1, na.rm = T))
conf_ints_hisp = cbind(conf_ints_hisp, "ces" = c(w - quantile(w_ces_hisp-w, 1-alpha/2), w, w - quantile(w_ces_hisp-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_hisp_ces_adj, sum(data$ces_dac_adj == 1, na.rm = T))
conf_ints_hisp = cbind(conf_ints_hisp, "ces_adj" = c(w - quantile(w_ces_adj_hisp-w, 1-alpha/2), w, w - quantile(w_ces_adj_hisp-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_hisp_cejst, sum(data$cejst_dac == 1, na.rm = T))
conf_ints_hisp = cbind(conf_ints_hisp, "cejst" = c(w - quantile(w_cejst_hisp-w, 1-alpha/2), w, w - quantile(w_cejst_hisp-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_hisp_eji, sum(data$eji_dac == 1, na.rm = T))
conf_ints_hisp = cbind(conf_ints_hisp, "eji" = c(w - quantile(w_eji_hisp-w, 1-alpha/2), w, w - quantile(w_eji_hisp-w, alpha/2)))

w = solve_wallenius(m1, m2, above_med_hisp_trivariate, sum(data$univariate_dac == 1, na.rm = T))
conf_ints_hisp = cbind(conf_ints_hisp, "trivariate" = c(w - quantile(w_trivariate_hisp-w, 1-alpha/2), w, w - quantile(w_trivariate_hisp-w, alpha/2)))


# Write the confidence interval data ----------------------------------------
toplot_a <- as.data.frame(t(conf_ints_black)) %>%
  rename("val" = "V2",
         "lower" = "97.5%",
         "upper" = "2.5%")
toplot_a$tool <- row.names(toplot_a)
write_csv(toplot_a, "data/data_processed/hypergeometric_a.csv")

toplot_b <- as.data.frame(t(conf_ints_hisp)) %>%
  rename("val" = "V2",
         "lower" = "97.5%",
         "upper" = "2.5%")
toplot_b$tool <- row.names(toplot_b)
write_csv(toplot_b, "data/data_processed/hypergeometric_b.csv")

