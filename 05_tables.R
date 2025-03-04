## ------------------------------------------------------- ##
                      # Table Creation
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# Purpose:
## Make publication-quality tables

# Pre-Requisites:
## This script assumes you've run the following scripts:
### - "02_stats-prep.R"
### - "03a_stats.R"

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Make a folder for exporting graphs
dir.create(path = file.path("data", "tables"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ----------------------------------------- ##
          # Stat Results Tables ----
## ----------------------------------------- ##

# Read in relevant table(s)
mean_stats_v1 <- read.csv(file = file.path("data", "stats-results", "avg_response_DSi_results.csv"))
perc_stats_v1 <- read.csv(file = file.path("data", "stats-results", "perc_change_DSi_results.csv"))

# Check structure
dplyr::glimpse(mean_stats_v1)
dplyr::glimpse(perc_stats_v1)

# Wrangle mean response
mean_stats_v2 <- mean_stats_v1 %>% 
  # Add response column
  dplyr::mutate(Response = "Mean DSi Concentration (uM)") %>% 
  # Strealine statistical information
  dplyr::mutate(`F Statistic` = round(f_stat, digits = 1),
                `P Value` = ifelse(round(p_value, digits = 3) == 0,
                                   yes = "P < 0.0001",
                                   no = as.character(round(p_value, digits = 3))) ) %>% 
  # Clean up 'term' column
  dplyr::mutate(term = gsub("scaled_mean_", "Scaled Mean ", term),
                term = gsub("npp_kgC.m2.year", "NPP (kg C/m2/year)", term),
                term = gsub("precip_mm.per.day", "Precipitation (mm/day)", term),
                term = gsub("snow_max.prop.area", "Snow (Max Proportion Area)", term),
                term = gsub("temp_degC", "Temperature (Celsius)", term),
                term = gsub("P_Conc_uM", "P Concentration (uM)", term),
                term = gsub("Discharge_cms", "Discharge (cms)", term) ) %>% 
  # Pare down to only desired columns
  dplyr::select(Response, Explanatory = term, `F Statistic`, `P Value`)

# Check structure
dplyr::glimpse(mean_stats_v2)

# Tidy the percent change results in the same way
perc_stats_v2 <- perc_stats_v1 %>% 
  dplyr::mutate(Response = "Percent Change DSi Concentration") %>% 
  dplyr::mutate(`F Statistic` = round(f_stat, digits = 1),
                `P Value` = ifelse(round(p_value, digits = 3) == 0,
                                   yes = "P < 0.0001",
                                   no = as.character(round(p_value, digits = 3))) ) %>% 
  dplyr::mutate(term = gsub("scaled_slope_", "Scaled Change in ", term),
                term = gsub("npp_kgC.m2.year", "NPP (kg C/m2/year)", term),
                term = gsub("precip_mm.per.day", "Precipitation (mm/day)", term),
                term = gsub("snow_max.prop.area", "Snow (Max Proportion Area)", term),
                term = gsub("temp_degC", "Temperature (Celsius)", term),
                term = gsub("P_Conc_uM", "P Concentration (uM)", term),
                term = gsub("Discharge_cms", "Discharge (cms)", term) ) %>% 
  dplyr::select(Response, Explanatory = term, `F Statistic`, `P Value`)

# Check that out
perc_stats_v2

# Combine the two data objects
stats_out <- dplyr::bind_rows(mean_stats_v2, perc_stats_v2)

# Final structure check
dplyr::glimpse(stats_out)

# Export
write.csv(stats_out, na = '', row.names = F,
          file = file.path("data", "tables", "summary-table_statistical-results.csv"))

## ----------------------------------------- ##
        # Pairwise Results Tables ----
## ----------------------------------------- ##

# Read in relevant table(s)
for(pair_name in dir(path = file.path("data", "stats-results"), pattern = "_results_pairwise.csv")){
  
  # Progress message
  message("Cleaning up '", pair_name, "'")

  # Read in the file
  pair_raw <- read.csv(file = file.path("data", "stats-results", pair_name))
  
  # Wrangle it as needed
  pair_tidy <- pair_raw %>% 
    dplyr::mutate(
      contrast = gsub(pattern = "Goverment", replacement = "Government", 
                      x = contrast),
      t_value = round(x = t.ratio, digits = 2),
      p_value = ifelse(test = p.value < 0.001,
                       yes = "P < 0.001",
                       no = round(x = p.value, digits = 3))
      ) %>% 
    dplyr::select(term, contrast, t_value, p_value)
  
  # Make a tidy name for this table
  new_name <- paste0("summary-table_", pair_name)

  # Export it under this name
  write.csv(x = pair_tidy, row.names = F, na = '',
            file = file.path("data", "tables", new_name))
  
} # Close loop

# End ----
