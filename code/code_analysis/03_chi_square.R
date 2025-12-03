## ---------------------------
##
## Script name: 03_chi_square
##
## Purpose of script:
##    This script determines, for block groups, whether DAC classifications from metrics (CES, CES
##    adjusted, CEJST, EJI, and New Jersey-like) result in different distributions of race, then ethnicity
##    data for people in DACs.
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
library(tidyverse)
library(gplots) # balloonplot

# Import data ---------------------------
data <- read_csv("data/data_processed/metrics_tract.csv") %>%
  dplyr::select(nonhispanic_white, nonhispanic_black, nonhispanic_american_indian_alaska_native, 
                nonhispanic_asian, nonhispanic_native_hawaiian_pacific_islander, 
                nonhispanic_other, nonhispanic_two_or_more, hispanic,
                univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac)%>%
  mutate(nonhispanic_other= nonhispanic_other+nonhispanic_two_or_more) %>%
  dplyr::select(-nonhispanic_two_or_more)

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

dac_data <- rbind(univariate_dac_data, ces_dac_data, ces_dac_adj_data, eji_dac_data, cejst_dac_data) %>%
  dplyr::select(-c(univariate_dac, ces_dac, ces_dac_adj, eji_dac, cejst_dac))

# create contingency table
contingency_table <- dac_data %>%
  group_by(dac) %>%
  summarise(across(everything(), sum))
rows = contingency_table$dac
contingency_table <- contingency_table %>%
  dplyr::select(-dac) 
rownames(contingency_table) = rows
contingency_table <- contingency_table %>%
  as.matrix(keep.rownames = TRUE) %>%
  as.table(keep.rownames = TRUE)

# Run Chi-Square Test ---------------------------
chisq <- chisq.test(contingency_table)
chisq
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

# Evaluate observed-expected (expected for statewide demographics) ---------------------------
# totals <- data %>%
#   dplyr::select(white, black, american_indian_alaska_native, asian, 
#                 native_hawaiian_pacific_islander, other, two_or_more) %>%
#   summarise(across(everything(), sum))
# totals <- totals/sum(totals)
# 
# observed_expected <- dac_data %>%
#   group_by(dac) %>%
#   summarise(across(everything(), sum))
# rows = observed_expected$dac
# observed_expected <- observed_expected%>%
#   dplyr::select(-dac) 
# rownames(observed_expected) = rows
# obs_ex2 <- scale(t(observed_expected), center = FALSE, 
#                  scale = rowSums(observed_expected)) %>%
#   t()
# 
# sweep(b, 2, c(0,0,x))
# sweep(obs_ex2, 2, c(totals))

# we are interested in how many times more or less likely you are to be classified in a disadvantaged 
# community based on race. P(in disadvantaged community|race) = P(in disadvantaged community and race)/P(race)
# ex <- (obs_ex2)/totals[col(obs_ex2)]
# to_plot <- as.data.frame(t(ex)) %>%
#   mutate(x = rownames(t(ex)))
# library(reshape2)
# to_plot <- melt(to_plot, id = c("x")) 
# 
# ggplot(to_plot, aes(x = x, y = value, group = variable, color = variable))+
#   geom_point() +
#   theme_classic() +
#   geom_hline(yintercept = 1) +
#   xlab("Metric") +
#   ylab("Proportion in DACs/Proportion in state")
