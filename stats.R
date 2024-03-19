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
  unique() %>%
  # Drop McMurdo
  dplyr::filter(LTER != "MCM")

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
                                abs(Latitude) + elevation_mean_m + land_total_forest +
                                land_barren_or_sparsely_vegetated,
                              data = si_conc, iter = 999)

# Get ANOVA table for that model
si_conc_aov1 <- anova(si_conc_mod1, effect.type = "F",
                      error = c(
                        "land_total_forest", # 'random' effect for LTER
                        "LTER", # random effect for 'slope_temp_degC'
                        "LTER", # random effect for 'slope_precip_mm.per.day'
                        "LTER", # random effect for 'slope_npp_kg.C.m2.year'
                        "LTER", # random effect for 'slope_evapotrans_kg.m2'
                        "LTER", # random effect for 'slope_snow_max.prop.area'
                        "LTER", # random effect for 'slope_snow_num.days'
                        "LTER", # random effect for 'Latitude'
                        "LTER", # random effect for 'elevation_mean_m'
                        "LTER", # random effect for 'land_total_forest'
                        "LTER")) # random effect for 'land_barren_or_sparsely_vegetated'

# Specifying "Residuals" in error argument means NO random effect is used
# Specifying any other fixed effect name means that is used as a random effect *for that term*

# Summarize that output (and check it out)
( si_conc_table1 <- aov_process(si_conc_aov1) )

# Export locally
write.csv(x = si_conc_table1, row.names = F, na = '',
          file = file.path("stats_results", "annual_DSi_conc_slope.csv"))

# Exploratory graph
ggplot(si_conc, aes(x = abs(Latitude), y = slope_estimate)) +
  geom_point(pch = 21, size = 3, fill = "blue") +
  geom_smooth(method = "lm", se = F, formula = "y ~ x", color = "black") +
  labs(x = "Latitude", y = "Slope of Si Conc. (uM) / Years") +
  supportR::theme_lyon()

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

# Get ANOVA table for that model
si_conc_aov2 <- anova(si_conc_mod2, effect.type = "F")

# Summarize that output (and check it out)
( si_conc_table2 <- aov_process(si_conc_aov2) )

# Export locally
write.csv(x = si_conc_table2, row.names = F, na = '',
          file = file.path("stats_results", "annual_DSi_conc_percchange.csv"))

## ----------------------------------------- ##
# Within-LTER Tests ----
## ----------------------------------------- ##

# Subset to a particular LTER
one_lter <- dplyr::filter(si_conc, LTER == "NIVA")

# Fit model
test_mod <- RRPP::lm.rrpp(percent_change ~ Stream_Name,
                          data = one_lter, iter = 999)

# Assess model
summary(test_mod, formula = F)

# Get pairwise comparisons
test_pw <- RRPP::pairwise(fit = test_mod, groups = one_lter$Stream_Name)
summary(test_pw)

# End ----
