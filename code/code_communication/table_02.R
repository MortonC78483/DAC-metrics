## ---------------------------
##
## Script name: table_02
##
## Purpose of script:
##    This script formats the table of kappa statistics
##
## Author: Claire Morton
##
## Date Created: 2023-02-21
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

# Import data ---------------------------
data <- read_csv("data/data_interim/table_kappa.csv")
names = c("Trivariate", "CES", "CES+", "EJI", "CEJST")
rownames = names

data <- data %>%
  select(-c(...1))
colnames(data) = names
data <- round(data, 2)
rownames(data) = rownames


# Export table ---------------------------
stargazer(data, 
          summary=FALSE, rownames=TRUE,
          type = "html", out = "outputs/tables/table_kappa.html")
