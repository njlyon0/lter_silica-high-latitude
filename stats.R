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
si_conc <- read.csv(file = file.path("tidy_data", "stats-ready_annual_Conc_uM_DSi_bw5.csv")) 
names(si_conc)

Driver1 <- si_conc %>%
  # Drop information not useful for regression model
  # When doing non-conc data need to change "conc_uM" to appropriate term
  dplyr::select(-relative_Year, -Conc_uM, -season:-Year, -temp_degC, -section:-section_end, -F_statistic:-slope_direction,
                -term_statistic:-term_p_value, -temp_degC, -sd_temp_degC, -precip_mm.per.day, -sd_precip_mm.per.day,
                -npp_kg.C.m2.year, -sd_npp_kg.C.m2.year, -evapotrans_kg.m2,
                -sd_evapotrans_kg.m2, -snow_max.prop.area, sd_snow_max.prop.area, -snow_num.days, -sd_snow_num.days) %>%
  #drop even more columns
  dplyr::select(-sizer_bandwidth, -sd_response, -section_duration, -slope_std_error,
                -land_evergreen_needleleaf_forest, -land_evergreen_broadleaf_forest, -land_mixed_forest, 
                -land_deciduous_broadleaf_forest, -land_deciduous_needleleaf_forest, -sd_snow_max.prop.area) %>%
  # Drop non-unique rows
  unique() %>%
  # Drop McMurdo b/c don't have driver data for that
  dplyr::filter(LTER != "MCM")

# Check structure
dplyr::glimpse(Driver1)
names(Driver1)


##========================================
#first isolate numeric data for scaling and checking for covariation and skew
##========================================

#isolate numeric data
Driver_numeric <- Driver1 %>%
  select(sizer_groups, where(is.numeric)) 
  
names(Driver_numeric)

#remove response variables and unique identifyer and examine coorelation among predictors

#elevation highly correlated with tundra and forest so just have forest here
#removed wetland, developed, cropland b/c many outliers
Driver_numeric_land <- Driver_numeric %>%
  select(-sizer_groups, -percent_change, -slope_estimate) %>%
  #pull out just the land and lat to see if related
  select(Latitude, contains("land_")) %>%
  select(-land_barren_or_sparsely_vegetated, -land_cropland, -land_urban_and_built_up_land, 
         -land_tundra, -land_wetland) %>%
  pairs ()
#keeping just Latitude, total forest, shrub-grassland
#except below we have to remove Lat and total forest b/c correlated with mean temp

Driver_numeric_meterol <- Driver_numeric %>%
  select(-sizer_groups, -percent_change, -slope_estimate) %>%
  #pull out lat and elevation
  select(-mean_response, -elevation_mean_m) %>%
  #pull out just the land and lat to see if related
  select(contains("mean_")) %>%
  #temp correlated with basically everything! just keep temp
  select(-mean_precip_mm.per.day, -mean_npp_kg.C.m2.year, -mean_snow_num.days) %>%
  pairs ()

Driver_numeric_meterol2 <- Driver_numeric %>%
  select(-sizer_groups, -percent_change, -slope_estimate) %>%
  #pull out just the land and lat to see if related
  select(contains("slope_")) %>%
  #remove slope of num snow days b/c correlated with slope of snow max prop area
  select(-slope_snow_num.days) %>%
  pairs ()

#so left with 8 predictors
names(Driver_numeric)

#need to add drainage area back in here but need to transform it I think first
Driver_num_final <- Driver_numeric %>%
  select(mean_temp_degC, land_shrubland_grassland, contains("slope_")) %>%
  #remove slope of num snow days b/c correlated with slope of snow max prop area
  select(-slope_snow_num.days) %>%
  pairs ()


#=======================================================
#below is old code - haven't tried this yet b/c still dealing with getting data ready...
#scaling data - removing first column which doesn't need to be scaled
Driver_Scaled <- as.data.frame(scale(x = Driver_numeric[-1], center = T, scale = T)) 

Driver_Scaled_Renamed <- supportR::safe_rename(data = Driver_Scaled, bad_names = names(Driver_Scaled), 
                                                  good_names = paste0("scaled_", names(Driver_Scaled))) %>%
  mutate(sizer_groups = Driver_numeric$sizer_groups)

#now that there is a column to join by, re-join with non-numeric data
Driver_Scaled2 <- Driver1 %>%
  left_join(y = Driver_Scaled_Renamed, by = "sizer_groups")

#remove duplicative columns so not scaled and scaled for same data
#now join with non-numeric data



#old model below - haven't re-run w/ scaled data
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
