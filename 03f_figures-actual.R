## ------------------------------------------------------- ##
# Figure Creation - Actual
## ------------------------------------------------------- ##
# Purpose:
## Make publication-quality figures
## THAT ARE ACTUALLY INCLUDED IN THE MAIN TEXT OF THE PAPER

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script


# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cowplot, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "figures_actual"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

#  End ----
