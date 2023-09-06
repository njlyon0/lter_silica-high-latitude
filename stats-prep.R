## ------------------------------------------------------- ##
            # Statistics & Visualization Prep
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# PURPOSE:
## Integrate basin characteristics / climatic "drivers" with the WRTDS data used in SiZer
## Do general wrangling operations required for some statistics / visualization

# Pre-Requisites:
## This script assumes you've run one of the "...-workflow.R" scripts
## And have the relevant output in a "sizer_outs" folder

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Make a folder for combined data / downloading drivers
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Identify / download the driver data
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK")) %>%
  dplyr::filter(name == "all-data_si-extract.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("tidy_data", .$name))

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
          # Driver Data Prep ----
## ----------------------------------------- ##

# Read in the driver data as it is
drivers_v1 <- read.csv(file = file.path("tidy_data", "all-data_si-extract.csv"))

# Check structure
dplyr::glimpse(drivers_v1)

# Split off the static driver information
static_v1 <- drivers_v1 %>%
  dplyr::select(LTER, Stream_Name, 
                elevation_mean_m,
                # dplyr::starts_with("elevation_"), 
                major_rock, 
                # dplyr::starts_with("rocks_"),
                major_land, 
                # dplyr::starts_with("land_"),
                major_soil
                # dplyr::starts_with("soil_")
                )

# Check that out
dplyr::glimpse(static_v1)

# Now split off the dynamic drivers (they'll require more wrangling
dynamic_v1 <- drivers_v1 %>%
  dplyr::select(LTER, Stream_Name, 
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
        # SiZer Output Retrieval ----
## ----------------------------------------- ##

# Define file name
sizer_filename <- "annual_Yield_kmol_yr_km2_DSi_bw5.csv"

# Read in SiZer output data
sizer_v1 <- read.csv(file = file.path("sizer_outs", sizer_filename))

# Check structure
dplyr::glimpse(sizer_v1)

## ----------------------------------------- ##
            # Integration Prep ----
## ----------------------------------------- ##

# First, check which LTERs are not in the SiZer data but are in the basin data
supportR::diff_check(old = unique(static_v1$LTER), new = unique(sizer_v1$LTER))
supportR::diff_check(old = unique(dynamic_v2$LTER), new = unique(sizer_v1$LTER))

# Drop any LTERs from the driver data that aren't in our SiZer data
static_v2 <- dplyr::filter(static_v1, LTER %in% unique(sizer_v1$LTER))
dynamic_v3 <- dplyr::filter(dynamic_v2, LTER %in% unique(sizer_v1$LTER))

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

# Re-check structure
dplyr::glimpse(sizer_v3)

## ----------------------------------------- ##
          # Quality of Life Tweaks ----
## ----------------------------------------- ##

# We'll want a few combination columns to exist for QoL purposes down the line
## Mostly to have easy things to map graphing aesthetics to but there are other benefits!

# Do desired wrangling
sizer_v4 <- sizer_v3 %>%
  # Drop ARC streams
  dplyr::filter(LTER != "ARC") %>%
  # Combine section with stream
  dplyr::mutate(sizer_groups = paste0(stream, "_", section), .before = dplyr::everything()) %>%
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
    T ~ paste0(slope_direction, "-", line_fit)), .after = line_fit) %>%
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great", 
                                            "NA", "NS")))
# Re-check structure
dplyr::glimpse(sizer_v4)

## ----------------------------------------- ##
       # Summarize Dynamic Drivers ----
## ----------------------------------------- ##

# We may want summarized variants of the dynamic drivers within sizer groups
sizer_v5 <- sizer_v4 %>%
  # Group by sizer groups 
  ## (note that if this was seasonal data it would also need to group by season)
  dplyr::group_by(sizer_groups) %>%
  # Do some calculations
  ## Get a 'relative Year' for each sizer group so all time series start at 1
  dplyr::mutate(relative_Year = 1:length(unique(Year)) , .after = Year) %>%
  ## Calculate average (and SD) dynamic drivers within sizer groups
  dplyr::mutate(mean_evapotrans_kg.m2 = mean(evapotrans_kg.m2, na.rm = T),
                sd_evapotrans_kg.m2 = sd(evapotrans_kg.m2, na.rm = T),
                mean_npp_kg.C.m2.year = mean(npp_kg.C.m2.year, na.rm = T),
                sd_npp_kg.C.m2.year = sd(npp_kg.C.m2.year, na.rm = T),
                mean_precip_mm.per.day = mean(precip_mm.per.day, na.rm = T),
                sd_precip_mm.per.day = sd(precip_mm.per.day, na.rm = T),
                mean_snow_max.prop.area = mean(snow_max.prop.area, na.rm = T),
                sd_snow_max.prop.area = sd(snow_max.prop.area, na.rm = T),
                mean_snow_num.days = mean(snow_num.days, na.rm = T),
                sd_snow_num.days = sd(snow_num.days, na.rm = T)) %>%
  # Remember to ungroup when done with these calculations
  dplyr::ungroup()

# Look at what that makes
dplyr::glimpse(sizer_v5)

## ----------------------------------------- ##
                  # Export ----
## ----------------------------------------- ##


# End ----
