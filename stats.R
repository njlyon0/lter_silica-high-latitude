## ------------------------------------------------------- ##
                  # Statistical Testing
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# PURPOSE:
## Do statistical testing to test hypotheses
## May include frequentist stats (P values) and model selection

# Pre-Requisites:
## This script assumes you've run the "stats-prep.R" script
## And have the relevant output in a "tidy_data" folder

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, RRPP)

# Make a folder for outputting results
dir.create(path = file.path("stats_results"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
# Silica Concentration ----
## ----------------------------------------- ##
# Read in ready data for this response variable
si_conc <- read.csv(file = file.path("tidy_data", "stats-ready_annual_Conc_uM_DSi_bw5.csv"))

# Check structure
dplyr::glimpse(si_conc)







# End ----
