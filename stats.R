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
          # Custom Function(s) ----
## ----------------------------------------- ##

# Neat little tool to summarize key bits of aov table
aov_process <- function(aov){
  
  # Get the summary table
  mod_table <- as.data.frame(aov$table)
  
  # Get the terms out as a real column (default is as rownames)
  mod_out <- mod_table %>%
    dplyr::mutate(Term = rownames(.), .before = dplyr::everything()) %>%
    # Rename P value while we're here
    dplyr::rename(P_Value = `Pr(>F)`)
  
  # Drop the rownames
  rownames(mod_out) <- NULL
  
  # Return that last object
  return(mod_out) }

## ----------------------------------------- ##
          # Silica Concentration ----
## ----------------------------------------- ##
# Read in ready data for this response variable
si_conc <- read.csv(file = file.path("tidy_data", "stats-ready_annual_Conc_uM_DSi_bw5.csv")) %>%
  # Drop annual information
  dplyr::select(-Year:-Conc_uM, -temp_degC, -precip_mm.per.day, -npp_kg.C.m2.year, 
                -evapotrans_kg.m2, -snow_max.prop.area, -snow_num.days) %>%
  # Drop non-unique rows
  unique()

# Check structure
dplyr::glimpse(si_conc)

# See how many line chunks (i.e., SiZer groups) per Stream_Name
si_conc %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarize(response_ct = length(unique(sizer_groups))) %>%
  dplyr::pull(response_ct) %>%
  unique()

# Fit a model of interest
si_conc_mod1 <- RRPP::lm.rrpp(slope_estimate ~ LTER +
                                ## Dynamic drivers
                                slope_temp_degC + slope_precip_mm.per.day +
                                slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
                                slope_snow_max.prop.area + slope_snow_num.days +
                                ## Static drivers
                                Latitude + elevation_mean_m + land_total_forest +
                                land_barren_or_sparsely_vegetated,
                              data = si_conc, iter = 999)

# Get ANOVA table for that model
si_conc_aov1 <- anova(si_conc_mod1, effect.type = "F")

# Summarize that output (and check it out)
( si_conc_table1 <- aov_process(si_conc_aov1) )

# Export locally
write.csv(x = si_conc_table1, row.names = F, na = '',
          file = file.path("stats_results", "annual_DSi_conc_slope.csv"))

# Do the same model for percent change
si_conc_mod2 <- RRPP::lm.rrpp(percent_change ~ LTER +
                                ## Dynamic drivers
                                slope_temp_degC + slope_precip_mm.per.day +
                                slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
                                slope_snow_max.prop.area + slope_snow_num.days +
                                ## Static drivers
                                Latitude + elevation_mean_m + land_total_forest +
                                land_barren_or_sparsely_vegetated,
                              data = si_conc, iter = 999)

# Summarize that output (and check it out)
( si_conc_aov2 <- aov_process(si_conc_mod2) )

# Export locally
write.csv(x = si_conc_aov2, row.names = F, na = '',
          file = file.path("stats_results", "annual_DSi_conc_percchange.csv"))

# End ----
