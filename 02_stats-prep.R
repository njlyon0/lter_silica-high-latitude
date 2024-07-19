## ------------------------------------------------------- ##
# Statistics & Visualization Prep
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Integrate basin characteristics / climatic "drivers" with the WRTDS data used in SiZer
## Do general wrangling operations required for some statistics / visualization

# Pre-Requisites:
## This script assumes you've run the "01_sizer-workflow.R" script

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, magrittr, supportR)

# Make a folder for combined data / downloading drivers
dir.create(path = file.path("drivers"), showWarnings = F)
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Identify / download the driver data
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK")) %>%
  dplyr::filter(name == "all-data_si-extract.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("drivers", .$name))

# Identify / download site reference table (for latitude info)
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
  dplyr::filter(name == "Site_Reference_Table") %>%
  googledrive::drive_download(file = .$id, overwrite = T, type = "csv",
                              path = file.path("drivers", .$name))

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
          # SiZer Output Selection ----
## ----------------------------------------- ##

# This script can handle either annual or seasonal data but you must make that choice here

# Define file name
sizer_filename <- "annual_Conc_uM_DSi_bw5.csv"
# sizer_filename <- "seasonal_Yield_kmol_yr_km2_DSi_bw5.csv"
#sizer_filename <- "monthly_Conc_uM_DSi_bw5.csv"

# Read in SiZer output data
sizer_v1 <- read.csv(file = file.path("sizer_outs", sizer_filename))

# If the data doesn't have a "season" column, add one
if(!"season" %in% names(sizer_v1)){
  sizer_v1 %<>%
    dplyr::mutate(season = "X", .before = chemical) }
# Note: `%<>%` is a "hinge assignment pipe"
## Equivalent to `df <- df %>% ...`

# If the data doesn't have a "Month" column, add that too
if(!"Month" %in% names(sizer_v1)){
  sizer_v1 %<>%
    dplyr::mutate(Month = "X", .before = chemical) }

# Check structure
dplyr::glimpse(sizer_v1)

##--------------------------------------------------##
#WRTDS output selection - grab P data
##--------------------------------------------------##

data_v0<-readr::read_csv('Full_Results_WRTDS_annual.csv')
names(data_v0)

#select just P data and stream
data_v1 <- data_v0 %>%
  dplyr::select(Stream_Name, Year, chemical, Conc_uM, FNConc_uM, Discharge_cms) %>%
  dplyr::filter(chemical=="P")

head(data_v1)

## ----------------------------------------- ##
          # Driver Data Prep ----
## ----------------------------------------- ##

# Read in the driver data as it is
drivers_v1 <- read.csv(file = file.path("drivers", "all-data_si-extract_2_20240623.csv"))

# Check structure
dplyr::glimpse(drivers_v1)

# Split off the static driver information
static_v1 <- drivers_v1 %>%
  dplyr::select(LTER, Stream_Name, 
                elevation_mean_m, major_rock, 
                major_land, dplyr::starts_with("land_"),
                major_soil)

# Check that out
dplyr::glimpse(static_v1)

# Now split off the dynamic drivers (they'll require more wrangling
dynamic_v1 <- drivers_v1 %>%
  dplyr::select(LTER, Stream_Name, dplyr::starts_with("temp_"),
                dplyr::starts_with("snow_"), dplyr::starts_with("evapotrans_"),
                dplyr::starts_with("npp_"), dplyr::starts_with("precip")) %>%
  # Drop monthly information of retained dynamic drivers
  dplyr::select(-dplyr::contains("_jan_"), -dplyr::contains("_feb_"), -dplyr::contains("_mar_"),
                -dplyr::contains("_apr_"), -dplyr::contains("_may_"), -dplyr::contains("_jun_"),
                -dplyr::contains("_jul_"), -dplyr::contains("_aug_"), -dplyr::contains("_sep_"),
                -dplyr::contains("_oct_"), -dplyr::contains("_nov_"), -dplyr::contains("_dec_"))

# Check structure
dplyr::glimpse(dynamic_v1)

# Need to summarize this to join appropriately with the SiZer data
dynamic_v2 <- dynamic_v1 %>%
  # Reshape this data into long format for ease of wrangling
  tidyr::pivot_longer(cols = -LTER:-Stream_Name) %>%
  # Clean up the units part of the old column names
  dplyr::mutate(name = gsub(pattern = "_num_days", replacement = "_num.days", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_mm_per_day", replacement = "_mm.per.day", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_kgC_m2_year", replacement = "_kg.C.m2.year", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_kg_m2", replacement = "_kg.m2", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_max_prop_area", replacement = "_max.prop.area", x = name)) %>%
  # Break the old name column into its component parts
  tidyr::separate_wider_delim(cols = name, delim = "_", names = c("driver", "Year", "units")) %>%
  # Recombine the driver and units columns
  dplyr::mutate(name_actual = paste(driver, units, sep = "_"), .before = driver) %>%
  dplyr::select(-driver, -units) %>%
  # Make "Year" numeric
  dplyr::mutate(Year = as.numeric(Year)) %>%
  # Average the values within our grouping variables
  dplyr::group_by(dplyr::across(-c(value))) %>%
  dplyr::summarize(value = mean(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Reshape back into wide format with the new name column!
  tidyr::pivot_wider(names_from = name_actual, values_from = value, values_fill = NA)

# Re-check structure
dplyr::glimpse(dynamic_v2)

## ----------------------------------------- ##
        # Driver Integration Prep ----
## ----------------------------------------- ##

# First, check which LTERs are not in the SiZer data but are in the basin data
supportR::diff_check(old = unique(static_v1$LTER), new = unique(sizer_v1$LTER))
supportR::diff_check(old = unique(dynamic_v2$LTER), new = unique(sizer_v1$LTER))

# Drop any LTERs from the driver data that aren't in our SiZer data
static_v2 <- dplyr::filter(static_v1, LTER %in% unique(sizer_v1$LTER))
dynamic_v3 <- dplyr::filter(dynamic_v2, LTER %in% unique(sizer_v1$LTER))

#Drop any streams from WRTDS data that aren't in SiZer data
WRTDS_v3 <- dplyr::filter(data_v1, Stream_Name %in% unique(sizer_v1$Stream_Name))

# Check that fixed the coarsest mismatch
supportR::diff_check(old = unique(static_v2$LTER), new = unique(sizer_v1$LTER))
supportR::diff_check(old = unique(dynamic_v3$LTER), new = unique(sizer_v1$LTER))

# Next, check for any streams that are in drivers but not SiZer and vice versa
## Waited 'til we dropped LTER mismatches to make this result easier to quickly scan
supportR::diff_check(old = unique(static_v2$Stream_Name), new = unique(sizer_v1$Stream_Name))
supportR::diff_check(old = unique(dynamic_v3$Stream_Name), new = unique(sizer_v1$Stream_Name))

# Drop any mismatched streams from the basin data
static_v3 <- dplyr::filter(static_v2, Stream_Name %in% sizer_v1$Stream_Name)
dynamic_v4 <- dplyr::filter(dynamic_v3, Stream_Name %in% sizer_v1$Stream_Name)

# Re-check stream mismatches (should just be McMurdo streams)
supportR::diff_check(old = unique(static_v3$Stream_Name), new = unique(sizer_v1$Stream_Name))
supportR::diff_check(old = unique(dynamic_v4$Stream_Name), new = unique(sizer_v1$Stream_Name))

# Note that these steps aren't totally needed because we're going to do a "left" join
## But still good to be explicit about what streams don't have driver data
## So we're not caught unawares by some missing data they shouldn't be missing

## ----------------------------------------- ##
          # Driver Integration ----
## ----------------------------------------- ##

# Combine the static driver data with the SiZer data!
sizer_v2 <- sizer_v1 %>%
  dplyr::left_join(y = static_v3, by = c("LTER", "Stream_Name"))

# Check structure
dplyr::glimpse(sizer_v2)

# Attach the dynamic drivers too
sizer_v3 <- sizer_v2 %>%
  dplyr::left_join(y = dynamic_v4, by = c("LTER", "Stream_Name", "Year"))

names(sizer_v3)

# Attach the WRTDS output too - this isnt working b/c already have a chemical column in here
sizer_v3.5 <-sizer_v3 %>%
  dplyr::left_join(y = WRTDS_v3, by = c("Stream_Name", "Year"))

# Re-check structure
dplyr::glimpse(sizer_v3.5)

## ----------------------------------------- ##
          # Latitude Integration ----
## ----------------------------------------- ##

# Read in the site reference table
site_info_v1 <- read.csv(file = file.path("drivers", "Site_Reference_Table.csv")) %>%
  # And subset to only LTERs in the SiZer data
  dplyr::filter(LTER %in% sizer_v3$LTER)

# Check structure
dplyr::glimpse(site_info_v1)

# Pare down the columns the bare minimum of needed information
site_info_v2 <- site_info_v1 %>%
  dplyr::select(LTER, Stream_Name, Latitude) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(site_info_v2)

# Check mismatch of streams with what is in the SiZer data
supportR::diff_check(old = unique(site_info_v2$Stream_Name), new = unique(sizer_v3$Stream_Name))

# Drop unwanted streams
site_info_v3 <- site_info_v2 %>%
  dplyr::filter(Stream_Name %in% unique(sizer_v3$Stream_Name))

# Any sites missing latitude?
site_info_v3 %>%
  dplyr::filter(is.na(Latitude))
## Any sites appearing here need to be edited **in the GoogleSheet "Site_Reference_Table"**
## See the Drive folder here: https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA

# Attach this to the SiZer data
sizer_v4 <- sizer_v3 %>%
  dplyr::left_join(y = site_info_v3, by = c("LTER", "Stream_Name")) %>%
  dplyr::relocate(Latitude, .after = term_p_value)

# Re-check structure
dplyr::glimpse(sizer_v4)

## ----------------------------------------- ##
          # Quality of Life Tweaks ----
## ----------------------------------------- ##

# We'll want a few combination columns to exist for QoL purposes down the line
## Mostly to have easy things to map graphing aesthetics to but there are other benefits!

# Do desired wrangling
sizer_v5 <- sizer_v4 %>%
  # Drop ARC streams
  dplyr::filter(LTER != "ARC") %>%
  # Combine section with stream
  dplyr::mutate(sizer_groups = paste0(stream, "_", section), 
                .before = dplyr::everything()) %>%
  # Categorize P values
  dplyr::mutate(significance = dplyr::case_when(
    is.na(test_p_value) ~ "NA",
    test_p_value < 0.05 ~ "sig",
    test_p_value >= 0.05 & test_p_value <= 0.1 ~ "marg",
    test_p_value > 0.1 ~ "NS"), .after = test_p_value) %>%
  # Categorize R2 too
  dplyr::mutate(line_fit = dplyr::case_when(
    is.na(r_squared) ~ "NA",
    r_squared < 0.3 ~ "bad",
    r_squared >= 0.3 & r_squared < 0.65 ~ "fine",
    r_squared >= 0.65 & r_squared < 0.8 ~ "good",
    r_squared >= 0.8 ~ "great"), .after = r_squared) %>%
  # Identify direction of slope
  dplyr::mutate(slope_direction = dplyr::case_when(
    is.na(slope_estimate) ~ "NA",
    slope_estimate < 0 ~ "neg",
    slope_estimate == 0 ~ "zero",
    slope_estimate > 0 ~ "pos"),
    .before = slope_estimate) %>%
  # Make combinations of direction + sig. and direction + line fit
  dplyr::mutate(dir_sig = dplyr::case_when(
    slope_direction == "NA" | significance == "NA" ~ "NA",
    significance == "NS" ~ "NS",
    T ~ paste0(slope_direction, "-", significance)), .after = significance) %>%
  dplyr::mutate(dir_fit = dplyr::case_when(
    slope_direction == "NA" | line_fit == "NA" ~ "NA",
    significance == "NS" ~ "NS",
    T ~ paste0(slope_direction, "-", line_fit)), .after = line_fit)

# Re-check structure
dplyr::glimpse(sizer_v5)

## ----------------------------------------- ##
       # Summarize Dynamic Drivers ----
## ----------------------------------------- ##

# Wrangle seasonal data
sizer_v6 <- sizer_v5 %>%
  # Group by sizer groups 
  dplyr::group_by(sizer_groups, season, Month) %>%
  # Do some calculations
  ## Get a 'relative Year' for each sizer group so all time series start at 1
  dplyr::mutate(relative_Year = row_number() , .after = Year) %>%
  ## Calculate average (and SD) dynamic drivers within sizer groups
  dplyr::mutate(mean_evapotrans_kg.m2 = mean(evapotrans_kg.m2, na.rm = T),
                sd_evapotrans_kg.m2 = sd(evapotrans_kg.m2, na.rm = T),
                mean_npp_kg.C.m2.year = mean(npp_kg.C.m2.year, na.rm = T),
                sd_npp_kg.C.m2.year = sd(npp_kg.C.m2.year, na.rm = T),
                mean_precip_mm.per.day = mean(precip_mm.per.day, na.rm = T),
                sd_precip_mm.per.day = sd(precip_mm.per.day, na.rm = T),
                mean_temp_degC = mean(temp_degC, na.rm = T),
                sd_temp_degC = sd(temp_degC, na.rm = T),
                mean_snow_max.prop.area = mean(snow_max.prop.area, na.rm = T),
                sd_snow_max.prop.area = sd(snow_max.prop.area, na.rm = T),
                mean_snow_num.days = mean(snow_num.days, na.rm = T),
                sd_snow_num.days = sd(snow_num.days, na.rm = T)) %>%
  # Remember to ungroup when done with these calculations
  dplyr::ungroup()

# Look at what that makes
dplyr::glimpse(sizer_v6)

## ----------------------------------------- ##
    # Calculate Dynamic Driver Slope ----
## ----------------------------------------- ##

# Make an empty list
dynamic_list <- list()

# Loop to get slope of dynamic drivers
for(chunk in unique(sizer_v6$sizer_groups)){
  
  # Message
  message("Calculating dynamic driver change for ", chunk)
  
  # Subset data
  sizer_sub <- dplyr::filter(sizer_v6, sizer_groups == chunk)
  
  # Loop across season within that chunk
  for(seas in unique(sizer_sub$season)){
    
    # Subset data again
    sizer_sub2 <- dplyr::filter(sizer_sub, season == seas)
    
    # Loop across month within that season
    for(mos in unique(sizer_sub2$Month)){
      
      # Subset once more
      sizer_sub3 <- dplyr::filter(sizer_sub2, Month == mos)
      
      # If there is more than one row in that subset
      if(nrow(sizer_sub3) > 1 & unique(sizer_sub3$LTER) != "MCM"){
        # Get slope of each dynamic driver within that sizer group
        ## ET
        if(!all(is.na(sizer_sub3$evapotrans_kg.m2))){
          et_slope <- as.data.frame(summary(lm(evapotrans_kg.m2 ~ relative_Year, 
                                               data = sizer_sub3))$coefficients)$Estimate[2] 
        } else { et_slope <- NA }
        ## NPP
        if(!all(is.na(sizer_sub3$npp_kg.C.m2.year))){
          npp_slope <- as.data.frame(summary(lm(npp_kg.C.m2.year ~ relative_Year, 
                                                data = sizer_sub3))$coefficients)$Estimate[2]
        } else { npp_slope <- NA }
        ## Precipitation
        if(!all(is.na(sizer_sub3$precip_mm.per.day))){
          ppt_slope <- as.data.frame(summary(lm(precip_mm.per.day ~ relative_Year, 
                                                data = sizer_sub3))$coefficients)$Estimate[2]
        } else { ppt_slope <- NA }
        ## Temperature
        if(!all(is.na(sizer_sub3$temp_degC))){
          temp_slope <- as.data.frame(summary(lm(temp_degC ~ relative_Year, 
                                                 data = sizer_sub3))$coefficients)$Estimate[2]
        } else { temp_slope <- NA }
        ## Snow Area
        if(!all(is.na(sizer_sub3$snow_max.prop.area))){
          snow1_slope <- as.data.frame(summary(lm(snow_max.prop.area ~ relative_Year, 
                                                  data = sizer_sub3))$coefficients)$Estimate[2]
        } else { snow1_slope <- NA }
        ## Snow Days
        if(!all(is.na(sizer_sub3$snow_num.days))){
          snow2_slope <- as.data.frame(summary(lm(snow_num.days ~ relative_Year, 
                                                  data = sizer_sub3))$coefficients)$Estimate[2]
        } else { snow2_slope <- NA }
        
        # Make name for list element
        list_name <- paste0(chunk, "_", seas, "_", mos)
        
        # Assemble a dataframe of this and add to the list
        dynamic_list[[list_name]] <- data.frame("sizer_groups" = chunk,
                                                "season" = seas,
                                                "Month" = mos,
                                                "slope_evapotrans_kg.m2" = et_slope,
                                                "slope_npp_kg.C.m2.year" = npp_slope,
                                                "slope_precip_mm.per.day" = ppt_slope,
                                                "slope_temp_degC" = temp_slope,
                                                "slope_snow_max.prop.area" = snow1_slope,
                                                "slope_snow_num.days" = snow2_slope)
      } } } }

# Unlist that
dynamic_slopes <- purrr::list_rbind(x = dynamic_list)
  
# Check structure
dplyr::glimpse(dynamic_slopes)

# Attach to data
sizer_v7 <- sizer_v6 %>%
  dplyr::left_join(y = dynamic_slopes, by = c("sizer_groups", "season", "Month")) %>%
  # Group dynamic drivers together
  dplyr::relocate(dplyr::contains("snow_num.days"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("snow_max.prop.area"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("evapotrans_kg.m2"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("npp_kg.C.m2.year"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("precip_mm.per.day"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("temp_degC"), .after = major_soil)
  
# Re-check structure
dplyr::glimpse(sizer_v7)

## ----------------------------------------- ##
        # Calculate 'Total Forest' ----
## ----------------------------------------- ##

# We want to calculate 'total forest' by summing relevant land cover categories
sizer_v8 <- sizer_v7 %>%
  # Flip land columns to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("land_")) %>%
  # If the value is NA, replace with 0
  dplyr::mutate(value = ifelse(is.na(value),
                                yes = 0, no = value)) %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = name, values_from = value) %>%
  # Move land columns back after 'major X' columns
  dplyr::relocate(dplyr::starts_with("land_"), 
                  .after = dplyr::starts_with("major_")) %>%
  # Sum forest columns into a 'total forest' column
  dplyr::mutate(land_total_forest = land_evergreen_needleleaf_forest +
                  land_evergreen_broadleaf_forest +
                  land_deciduous_broadleaf_forest +
                  land_deciduous_needleleaf_forest +
                  land_mixed_forest,
                .before = dplyr::starts_with("land_"))

# Re-check structure
dplyr::glimpse(sizer_v8)

## ----------------------------------------- ##
                  # Export ----
## ----------------------------------------- ##

# Re-name this object
stats_ready <- sizer_v8 
  # And choose a minimum chunk duration for inclusion
  #dplyr::filter(section_duration >= 5)

# Make a file name for this file
(ready_filename <- paste0("stats-ready_", sizer_filename))

# Save it locally
write.csv(x = stats_ready, na = "", row.names = F,
          file = file.path("tidy_data", ready_filename))

# End ----
