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
librarian::shelf(tidyverse, readxl, supportR)

# Clear environment
rm(list = ls())

# Load custom functions
for(fxn in dir(path = file.path("tools"), pattern = "fxn_")){
  source(file.path("tools", fxn))
}

## And remove loop index object from environment
rm(list = "fxn")

# Identify desired SiZer output
sizer_file <- "sizer-outs_annual_Conc_uM_DSi.csv"
# sizer_file <- "sizer-outs_seasonal_Conc_uM_DSi.csv"
# sizer_file <- "sizer-outs_monthly_Conc_uM_DSi.csv"

# Read in that SiZer output
sizer_v1 <- read.csv(file = file.path("data", sizer_file))

## ----------------------------------------- ##
          # Reference Table Prep ----
## ----------------------------------------- ##

# Read in the reference table
ref_v1 <- readxl::read_excel(path = file.path("data", "Site_Reference_Table.xlsx"))

# Process this for integration with the SiZer stuff
ref_v2 <- ref_v1 %>% 
  # Pare down to desired columns only
  dplyr::select(LTER, Stream_Name, Latitude, Longitude) %>% 
  # Remove unwanted streams / duplicate rows
  dplyr::filter(LTER %in% unique(sizer_v1$LTER)) %>% 
  dplyr::filter(Stream_Name %in% unique(sizer_v1$stream)) %>% 
  dplyr::distinct() %>% 
  # Remove any rows where Latitude is unknown
  dplyr::filter(!is.na(Latitude))

# Check structure
dplyr::glimpse(ref_v2)

## ----------------------------------------- ##
            # Driver Data Prep ----
## ----------------------------------------- ##

# Read in the driver data
drivers_v1 <- read.csv(file = file.path("data", "all-data_si-extract_2.csv"))

# Split off static drivers
static_v1 <- drivers_v1 %>%
  # Pare down to needed columns
  dplyr::select(LTER, Stream_Name, elevation_mean_m, major_rock, 
                major_land, major_soil, dplyr::starts_with("land_")) %>% 
  # Remove unwanted streams / duplicate rows
  dplyr::distinct() %>% 
  dplyr::filter(LTER %in% unique(sizer_v1$LTER)) %>% 
  dplyr::filter(Stream_Name %in% unique(sizer_v1$stream))

# Check structure
dplyr::glimpse(static_v1)

# Split off the dynamic ones too
dynamic_v1 <- drivers_v1 %>% 
  # Only desired columns
  dplyr::select(LTER, Stream_Name, dplyr::starts_with("temp_"),
                dplyr::starts_with("snow_"), dplyr::starts_with("evapotrans_"),
                dplyr::starts_with("npp_"), dplyr::starts_with("precip")) %>%
  # Drop monthly information of retained dynamic drivers
  dplyr::select(-dplyr::contains(c("_jan_", "_feb_", "_mar_", "_apr_",
                                   "_may_", "_jun_", "_jul_", "_aug_",
                                   "_sep_", "_oct_", "_nov_", "_dec_"))) %>% 
  # Remove unwanted streams / duplicate rows
  dplyr::filter(LTER %in% unique(sizer_v1$LTER)) %>% 
  dplyr::filter(Stream_Name %in% unique(sizer_v1$stream)) %>% 
  dplyr::distinct() %>% 
  # Reshape longer
  tidyr::pivot_longer(cols = -LTER:-Stream_Name) %>% 
  # Remove NA values
  dplyr::filter(!is.na(value)) %>% 
  # Standardize delimited between bits of information
  dplyr::mutate(
    name = gsub(pattern = "_num_days", replacement = "_num.days", x = name),
    name = gsub(pattern = "_max_prop_area", replacement = "_max.prop.area", x = name),
    name = gsub(pattern = "_kg_m2", replacement = "_kg.m2", x = name),
    name = gsub(pattern = "_kgC_m2_year", replacement = "_kgC.m2.year", x = name),
    name = gsub(pattern = "_mm_per_day", replacement = "_mm.per.day", x = name)) %>% 
  # Split that apart into the three components of each column name
  tidyr::separate_wider_delim(cols = name, delim = "_", 
                              names = c("driver", "Year", "units")) %>% 
  # Recombine the driver and units columns
  dplyr::mutate(name_actual = paste(driver, units, sep = "_"), .before = driver) %>%
  dplyr::select(-driver, -units) %>%
  # Make "Year" numeric
  dplyr::mutate(Year = as.numeric(Year)) %>%
  # Average the values within our grouping variables
  dplyr::group_by(dplyr::across(-c(value))) %>%
  dplyr::summarize(value = mean(value, na.rm = T),
                   .groups = "keep") %>%
  dplyr::ungroup() %>%
  # Reshape back into wide format with the new name column!
  tidyr::pivot_wider(names_from = name_actual, values_from = value, values_fill = NA)

# Check structure
dplyr::glimpse(dynamic_v1)

## ----------------------------------------- ##
          # WRTDS Output Prep ----
## ----------------------------------------- ##

# Flexibly determine the needed resolution of WRTDS output
if(stringr::str_detect(string = sizer_file, pattern = "annual")){
  wrtds_file <- "Full_Results_WRTDS_annual.csv"
} else { wrtds_file <- "Full_Results_WRTDS_monthly.csv" }

# Read in the specified WRTDS data
wrtds_v1 <- read.csv(file = file.path("data", wrtds_file))

# Wrangle these data to just the bits we want to use as covariates with SiZer outputs
wrtds_v2 <- wrtds_v1 %>% 
  # Pare down to just needed columns
  dplyr::select(LTER, Stream_Name, drainSqKm, Year, chemical, Conc_uM, FNConc_uM) %>% 
  # Remove the colons from ratio chemicals
  dplyr::mutate(chemical = gsub(pattern = ":", replacement = ".", x = chemical)) %>% 
  # Remove unwanted streams / duplicate rows
  dplyr::filter(LTER %in% unique(sizer_v1$LTER)) %>% 
  dplyr::filter(Stream_Name %in% unique(sizer_v1$stream)) %>% 
  dplyr::distinct() %>% 
  # Remove whichever chemical is the focus of the chosen SiZer outputs
  dplyr::filter(chemical != unique(sizer_v1$chemical)) %>%
  # Reshape longer
  tidyr::pivot_longer(cols = dplyr::contains("Conc_uM")) %>% 
  # Reassemble a new column name + drop older bits
  dplyr::mutate(name_fix = paste0(chemical, "_", name)) %>% 
  dplyr::select(-chemical, -name) %>% 
  # Average within existing groups
  dplyr::group_by(LTER) %>% 
  dplyr::group_by(dplyr::across(-c(value))) %>%
  dplyr::summarize(value = mean(value, na.rm = T),
                   .groups = "keep") %>%
  dplyr::ungroup() %>%
  # Remove invalid values
  dplyr::filter(value >= 0 & value <= Inf) %>% 
  # Reshape wider
  tidyr::pivot_wider(names_from = name_fix, values_from = value, values_fill = NA)

# Check structure
dplyr::glimpse(wrtds_v2)

# Prepare discharge for inclusion too
wrtds_disc <- wrtds_v1 %>% 
  # Pare down to just needed columns
  dplyr::select(LTER, Stream_Name, drainSqKm, Year, Discharge_cms) %>% 
  # Remove unwanted streams / duplicate rows
  dplyr::filter(LTER %in% unique(sizer_v1$LTER)) %>% 
  dplyr::filter(Stream_Name %in% unique(sizer_v1$stream)) %>% 
  dplyr::distinct() %>% 
  # Average discharge within groups
  dplyr::group_by(dplyr::across(-c(Discharge_cms))) %>%
  dplyr::summarize(Discharge_cms = mean(Discharge_cms, na.rm = T),
                   .groups = "keep") %>%
  dplyr::ungroup()

# Check structure
dplyr::glimpse(wrtds_disc)

# Combine discharge and non-focal chemical data
wrtds_v3 <- wrtds_v2 %>% 
  dplyr::left_join(y = wrtds_disc, by = c("LTER", "Stream_Name", "drainSqKm", "Year"))

# Check structure
dplyr::glimpse(wrtds_v3)

## ----------------------------------------- ##
            # Integrate Data ----
## ----------------------------------------- ##

# Combine the SiZer ouputs with the various data files we prepared above
## Note that each shares different columns with the SiZer outputs
sizer_v2 <- sizer_v1 %>% 
  # Rename 'stream' column for convenience
  dplyr::rename(Stream_Name = stream) %>% 
  # Integrate reference table
  dplyr::left_join(y = ref_v2, by = c("LTER", "Stream_Name")) %>% 
  # Integrate drivers (static & dynamic)
  dplyr::left_join(y = static_v1, by = c("LTER", "Stream_Name")) %>% 
  dplyr::left_join(y = dynamic_v1, by = c("LTER", "Stream_Name", "Year")) %>% 
  # Integrate WRTDS
  dplyr::left_join(y = wrtds_v3, by = c("LTER", "Stream_Name", "drainSqKm", "Year"))
  
# Check structure
dplyr::glimpse(sizer_v2)

# Wrangle that output with some quality-of-life improvements
sizer_v3 <- sizer_v2 %>% 
  # Rename stream column
  dplyr::rename(stream = Stream_Name) %>% 
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

# Check structure
dplyr::glimpse(sizer_v3)

## ----------------------------------------- ##
        # Summarize Covariates ----
## ----------------------------------------- ##

# Summarize co-variates within groups
sizer_covars <- sizer_v3 %>% 
  # Drop all but certain columns
  dplyr::select(sizer_groups:Year, 
                ## Dynamic drivers
                dplyr::ends_with(c("_kg.m2", "_kgC.m2.year", "_mm.per.day",
                                   "_max.prop.area", "_num.days", "_degC")),
                ## Non-focal chemicals
                dplyr::starts_with(c("DSi_", "NO3_", "DIN_", "NH4_", "NOx_",
                                     "P_", "Si.DIN_", "Si.P_")),
                ## Discharge information
                Discharge_cms) %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = -sizer_groups:-Year) %>% 
  # Filter out NAs
  dplyr::filter(!is.na(value)) %>% 
  # Summarize co-variates within groups
  dplyr::group_by(sizer_groups, sizer_bandwidth, LTER, stream, LTER_stream, 
                  drainSqKm, chemical, Month, season, name) %>% 
  dplyr::summarize(mean = mean(value, na.rm = T),
                   sd = sd(value, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Pivot *that* to long format
  tidyr::pivot_longer(cols = mean:sd, names_to = "stat") %>% 
  # Assemble new column names
  dplyr::mutate(name_actual = paste0(stat, "_", name)) %>% 
  # Drop unwanted columns
  dplyr::select(-name, -stat) %>% 
  # Reshape back to wide format
  tidyr::pivot_wider(names_from = name_actual, values_from = value)

# Check structure
dplyr::glimpse(sizer_covars)

# Do some small wrangling of the 'actual' data object
sizer_v4 <- sizer_v3 %>% 
  # Group and calculate relative year within groups
  dplyr::group_by(sizer_groups, season, Month) %>%
  dplyr::mutate(relative_Year = row_number() , .after = Year) %>%
  dplyr::ungroup() %>% 
  # Attach covariate information
  dplyr::left_join(y = sizer_covars, by = c("sizer_groups", "sizer_bandwidth", 
                                            "LTER", "stream", "LTER_stream", 
                                            "drainSqKm", "chemical", "Month", "season"))

# Check structure
dplyr::glimpse(sizer_v4)

## ----------------------------------------- ##
      # Calculate Covariate Slopes ----
## ----------------------------------------- ##

# For which covariates do you want to extract slope (against relative year)?
desired_vars <- c("evapotrans_kg.m2", "npp_kgC.m2.year", "precip_mm.per.day", 
                  "temp_degC", "snow_max.prop.area", "snow_num.days", "Discharge_cms",
                  "DSi_Conc_uM", "DSi_FNConc_uM", "NO3_Conc_uM", "NO3_FNConc_uM", 
                  "DIN_Conc_uM", "DIN_FNConc_uM", "NH4_Conc_uM", "NH4_FNConc_uM", 
                  "NOx_Conc_uM", "NOx_FNConc_uM",  "P_Conc_uM", "P_FNConc_uM", 
                  "Si.DIN_Conc_uM", "Si.DIN_FNConc_uM", "Si.P_Conc_uM", "Si.P_FNConc_uM")

# Strip out any not found in data
(actual_covars <- generics::intersect(x = desired_vars, y = names(sizer_v4)))

# Make an empty list for storing outputs
slope_list <- list()

# Loop across sizer groups
for(focal_gp in unique(sizer_v4$sizer_groups)){

  # Message
  message("Calculating covariate slope for ", focal_gp)
  
  # Subset to that group
  sizer_group_sub <- dplyr::filter(.data = sizer_v4, sizer_groups == focal_gp)
  
  # And across seasons
  for(focal_sea in unique(sizer_group_sub$season)){
    
    # Subset to that season
    sizer_season_sub <- dplyr::filter(.data = sizer_group_sub, season == focal_sea)
    
    # And across months
    for(focal_mon in unique(sizer_season_sub$Month)){
      
      # Subset to that month
      sizer_mo_sub <- dplyr::filter(.data = sizer_season_sub, Month == focal_mon)
      
      # Loop across desired response variables
      for(focal_var in actual_covars){
        
        # Extract slope for that covariate
        focal_slope <- get_slope(data = sizer_mo_sub, resp_var = focal_var)
        
        # Assemble a small output dataframe
        focal_out <- data.frame("sizer_groups" = focal_gp,
                                "season" = focal_sea,
                                "Month" = focal_mon,
                                "variable" = focal_var,
                                "slope" = focal_slope)
        
        # Assemble a unique name for this list element
        focal_list_name <- paste0(focal_gp, "_", focal_sea, "_", focal_mon, "_", focal_var)
        
        # Add to output list
        slope_list[[focal_list_name]] <- focal_out
        
      } # Close variable loop
    } # Close month loop
  } # Close season loop
} # Close sizer group loop

# Wrangle the list output of that loop
slope_out <- slope_list %>% 
  # Unlist that list
  purrr::list_rbind(x = .) %>% 
  # Reshape to wide format so each variable is its own column
  dplyr::mutate(name = paste0("slope_", variable)) %>% 
  dplyr::select(-variable) %>% 
  tidyr::pivot_wider(names_from = name, values_from = slope)

# Check structure
dplyr::glimpse(slope_out)

# Attach these to the core data object
sizer_v5 <- sizer_v4 %>% 
  dplyr::left_join(y = slope_out, by = c("sizer_groups", "season", "Month"))

# Check structure
dplyr::glimpse(sizer_v5)

## ----------------------------------------- ##
        # Calculate 'Total Forest' ----
## ----------------------------------------- ##

# Want to summarize across multiple land categories to just 'forest'
tot_forest <- sizer_v5 %>% 
  # Make NAs into zeros
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(test = is.na(.x) == T,
                                              yes = 0, no = .x))) %>% 
  # Sum 'forest' land categories
  dplyr::mutate(land_total_forest = land_evergreen_needleleaf_forest +
                  land_evergreen_broadleaf_forest +
                  land_deciduous_broadleaf_forest +
                  land_deciduous_needleleaf_forest +
                  land_mixed_forest) %>% 
  # Drop all columns except total forest and group columns
  dplyr::select(sizer_groups:season, land_total_forest) %>% 
  # Drop non-unique rows
  dplyr::distinct()
  
# Check structure
dplyr::glimpse(tot_forest)

# Attach these to the core data object
sizer_v6 <- sizer_v5 %>% 
  dplyr::left_join(y = tot_forest, 
                   by = c("sizer_groups", "sizer_bandwidth", "LTER", 
                          "stream", "LTER_stream", "drainSqKm", "chemical", 
                          "Month", "season"))

# Check structure
dplyr::glimpse(sizer_v6)

## ----------------------------------------- ##
            # Final Wrangling ----
## ----------------------------------------- ##

# Do final tidying before export
sizer_v7 <- sizer_v6 %>% 
  # Move related columns next to one another
  ## Drivers
  dplyr::relocate(dplyr::contains("land_"), .after = major_land) %>% 
  dplyr::relocate(dplyr::contains("snow_num.days"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("snow_max.prop.area"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("evapotrans_kg.m2"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("npp_kgC.m2.year"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("precip_mm.per.day"), .after = major_soil) %>%
  dplyr::relocate(dplyr::contains("temp_degC"), .after = major_soil) %>% 
  ## Discharge
  dplyr::relocate(dplyr::contains("Discharge_cms"), .after = slope_snow_num.days) %>% 
  ## Non-focal chemicals
  dplyr::relocate(dplyr::contains("Si.DIN_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("Si.DIN_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("Si.P_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("Si.P_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("NOx_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("NOx_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("NH4_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("NH4_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("DIN_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("DIN_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("NO3_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("NO3_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("P_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("P_FNConc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("DSi_Conc_uM"), .after = slope_Discharge_cms) %>% 
  dplyr::relocate(dplyr::contains("DSi_FNConc_uM"), .after = slope_Discharge_cms)

# Check structure
dplyr::glimpse(sizer_v7)

## ----------------------------------------- ##
                  # Export ----
## ----------------------------------------- ##

# Make a final object
stats_ready <- sizer_v7

# Check column names/order
names(stats_ready)

# Create an informative file name for this file
(ready_file <- gsub(pattern = "sizer-outs", replacement = "stats-ready", sizer_file))

# Save it locally
write.csv(x = stats_ready, na = "", row.names = F, file = file.path("data", ready_file))

# End ----
