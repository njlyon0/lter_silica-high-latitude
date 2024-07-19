## ------------------------------------------------------- ##
                    # `SiZer` Workflow
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Run SiZer workflow on data produced by WRTDS
## WRTDS = Weighted Regressions on Time, Discharge, and Season

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Need to force an install of HERON to get an updated version?
# devtools::install_github("lter/HERON", force = T)

# Silence `dplyr::summarize` message
options(dplyr.summarize.inform = FALSE)

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, SiZer, supportR, lter/HERON)

# Clear environment
rm(list = ls())

# Make needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)

# Download the data you need (or put it in the "data" folder manually)
## Note that you *must* have access to the relevant Shared Google Drive for this to work
# source("00_data-download.R")

# Load custom functions
for(fxn in dir(path = file.path("tools"), pattern = "fxn_")){
  source(file.path("tools", fxn))
}

## And remove loop index object from environment
rm(list = "fxn")

## ----------------------------------------- ##
          # General SiZer Prep ----
## ----------------------------------------- ##

# Specify data file
wrtds_file <- "Full_Results_WRTDS_monthly.csv"
# wrtds_file <- "Full_Results_WRTDS_annual.csv"

# Read in data & rename a column
wrtds_v1 <- read.csv(file = file.path("data", wrtds_file))

# Remove unwanted data / data that don't meet needed criteria
wrtds_v2 <- wrtds_v1 %>% 
  # Keep only LTERs at high latitudes
  dplyr::filter(LTER %in% c("MCM", "GRO", "NIVA", "Krycklan",
                            "Finnish Environmental Institute", 
                            "Canada", "Swedish Goverment")) %>% 
  # Drop some problem sites within wanted LTERs
  dplyr::filter(!Stream_Name %in% c("Site 69038", "Kymijoki Ahvenkoski 001", "Kymijoki Kokonkoski 014", "BEAVER RIVER ABOVE HIGHWAY 1 IN GLACIER NATIONAL PARK", "KICKING HORSE RIVER AT FIELD IN YOHO NATIONAL PARK", "SKEENA RIVER AT USK", "KOOTENAY RIVER ABOVE HIGHWAY 93 IN KOOTENAY NATIONAL PARK", "Helgean Hammarsjon", "Ronnean Klippan", "Morrumsan Morrum", "Lyckebyan Lyckeby", "Lagan Laholm", "Nissan Halmstad", "Atran Falkenberg", "Alsteran Getebro", "Eman Emsfors", "Viskan Asbro", "Gota Alv Trollhattan", "Rane alv Niemisel", "Raan Helsingborg")) %>% 
  # Calculate number of years per stream
  dplyr::group_by(LTER, Stream_Name) %>%
  dplyr::mutate(num_years = length(unique(Year)), .after = Year) %>%
  dplyr::ungroup() %>% 
  # Keep only streams with more years than some threshold
  dplyr::filter(num_years >= 12) %>% 
  # Drop the year number column
  dplyr::select(-num_years)

# Do some unit conversions / tidying
wrtds_v3 <- wrtds_v2 %>% 
  # Convert all of the 10^6 columns
  dplyr::mutate(
    dplyr::across(.cols = dplyr::contains("10_6k"),
                  .fns = ~ ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                  yes = (.x * 10^6), no = NA)
                  )) %>% 
  # Rename those columns to reflect their new units
  dplyr::rename_with(.fn = ~ gsub(pattern = "10_6k", replacement = "k", x = .x),
                     .cols = dplyr::contains("10_6k")) %>% 
  # Tidy the chemical ratio names
  dplyr::mutate(chemical = dplyr::case_when(
    chemical == "Si:DIN" ~ "Si_DIN",
    chemical == "Si:P" ~ "Si_P",
    TRUE ~ chemical)) %>% 
  # Rename stream column
  dplyr::rename(stream = Stream_Name)

# Check structure
dplyr::glimpse(wrtds_v3)

# Check that worked as desired
range(wrtds_v2$Yield_10_6kmol_yr_km2, na.rm = T)
range(wrtds_v3$Yield_kmol_yr_km2, na.rm = T)

# How many streams / LTER remaining?
wrtds_v3 %>% 
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = length(unique(stream)))

# Check lost/gained columns
supportR::diff_check(old = names(wrtds_v1), new = names(wrtds_v3))

# Duplicate the data object to reduce risk of accident
wrtds_v4 <- wrtds_v3

# If this dataset does not contain month/season columns (i.e., is annual) add them
if(!"Month" %in% names(wrtds_v3)){
  wrtds_v4 <- dplyr::mutate(.data = wrtds_v4, Month = "x", .before = Year)
}
if(!"season" %in% names(wrtds_v3)){
  wrtds_v4 <- dplyr::mutate(.data = wrtds_v4, season = "x", .before = Year)
}

# Check structure
dplyr::glimpse(wrtds_v4)

## ----------------------------------------- ##
          # Variable Selection ----
## ----------------------------------------- ##

# What is the temporal resolution of the WRTDS output data?
## *MUST* be one of "annual", "seasonal", or "monthly"
temporal_res <- "seasonal"

# Choose response/explanatory variables of interest & focal chemical
response <- "Conc_uM"
explanatory <- "Year"
element <- "DSi"

# Check that combination of variables works
var_check(data = wrtds_v4, chem = element, 
          resp_var = response, exp_var = explanatory)

# Prepare just the desired pieces of information
wrtds_focal <- wrtds_v4 %>% 
  dplyr::select(LTER:chemical, dplyr::starts_with(response)) %>% 
  dplyr::filter(chemical == element)

# Check structure
dplyr::glimpse(wrtds_focal)

# Create a folder for outputs
(output_dir = paste(temporal_res, response, element, sep = "_"))
dir.create(path = file.path(output_dir), showWarnings = F)

## ----------------------------------------- ##
          # Core SiZer Workflow ----
## ----------------------------------------- ##

# Make some empty lists to store different bits of information
data_list <- list()
statistic_list <- list()
estimate_list <- list()

# Loop across streams
for(place in unique(wrtds_focal$stream)){
# for(place in "Iijoki Raasakan voimal"){
  
  # Starting message
  message("Processing begun for ", element, " ", response, " at ", place)
  
  # Make a shorter place name
  place_short <- stringr::str_sub(string = place, start = 1, end = 8)
  
  # Subset to just this stream
  wrtds_place <- dplyr::filter(.data = wrtds_focal, stream == place)
  
  # Handle each temporal resolution separately
  ## Annual
  if(temporal_res == "annual"){
    
    # Simply duplicate existing object
    wrtds_core <- wrtds_place
    
    # Make some housekeeping objects that'll be useful in the processing script
    focal_season <- "x"
    focal_month <- "x"
    
    # And run workflow script
    source(file.path("tools", "flow_core-sizer-process.R"))
    
    ## Seasonal
  } else if(temporal_res == "seasonal"){
    
    # Make needed housekeeping object
    focal_month <- "x"
    
    # Process the WRTDS place object to get one row / season
    wrtds_place_v2 <- wrtds_place %>% 
      # Remove all instances where season info is missing
      dplyr::filter(!is.na(season)) %>%
      # Drop some unwanted columns
      dplyr::select(-nDays, -DecYear) %>% 
      # Flip to long format to get all response variables into a single column
      tidyr::pivot_longer(cols = dplyr::starts_with(response),
                          names_to = "var",
                          values_to = "value") %>%
      # Group by everything *except month* and average response values
      dplyr::group_by(LTER, stream, drainSqKm, season, Year, chemical, var) %>%
      dplyr::summarize(value = mean(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # Flip back to wide format
      tidyr::pivot_wider(names_from = var, values_from = value)
    
    # Loop across seasons
    for(focal_season in unique(wrtds_place_v2$season)){
      
      # Message current season
      message("Working on season: ", focal_season)
      
      # Creating a season-specific subset
      wrtds_core <- dplyr::filter(.data = wrtds_place_v2, season == focal_season)
      
      # *Then* running the workflow script
      source(file.path("tools", "flow_core-sizer-process.R"))
      
    } # Close season loop
    
    ## Monthly
  } else if(temporal_res == "monthly") {
    
    # Make needed housekeeping object
    focal_season <- "x"
    
    # Process the WRTDS place object to get one row / month
    wrtds_place_v2 <- wrtds_place %>% 
      # Remove all instances where month info is missing
      dplyr::filter(!is.na(Month)) %>%
      # Drop some unwanted columns
      dplyr::select(-nDays, -DecYear) %>% 
      # Flip to long format to get all response variables into a single column
      tidyr::pivot_longer(cols = dplyr::starts_with(response),
                          names_to = "var",
                          values_to = "value") %>%
      # Group by everything *except season* and average response values
      dplyr::group_by(LTER, stream, drainSqKm, Month, Year, chemical, var) %>%
      dplyr::summarize(value = mean(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # Flip back to wide format
      tidyr::pivot_wider(names_from = var, values_from = value)
    
    # Loop across months
    for(focal_month in unique(wrtds_place_v2$Month)){
      
      # Message current month
      message("Working on month: ", focal_month)
      
      # Creating a month-specific subset
      wrtds_core <- dplyr::filter(.data = wrtds_place_v2, Month == focal_month)
      
      # *Then* running the workflow script
      source(file.path("tools", "flow_core-sizer-process.R"))
      
    } # Close month loop
    
  } else {
    stop("Unrecognized temporal resolution! Replace with one of 'annual', 'seasonal', or 'monthly'")
  }
  
} # Close processing loop

## ----------------------------------------- ##
            # Process Outputs ----
## ----------------------------------------- ##

# Process the data first
data_actual <- purrr::list_rbind(x = data_list) %>% 
  # Tweak the '-Inf to Inf' entry for consistency with other dataframes and rename column
  dplyr::mutate(section = ifelse(groups == "(-Inf, Inf]",
                                 yes = "No inflection points", no = groups), 
                .after = LTER) %>%
  # Make end/start actually numbers and calculate duration of group
  dplyr::mutate(section_start = as.numeric(start),
                section_end = as.numeric(end),
                section_duration = section_end - section_start,
                .after = section_end) %>% 
  # Remove redundant columns
  dplyr::select(-groups, -start, -end) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(data_actual)

# Wrangle the estimate outputs
estimate_actual <- purrr::list_rbind(x = estimate_list) %>% 
  # Remove intercept information
  dplyr::filter(term != "(Intercept)") %>% 
  # Drop term column now that it's all "data[[x]]"
  dplyr::select(-term) %>%
  # Rename some columns for clarity
  dplyr::rename(std_error = std.error,
                term_statistic = statistic,
                term_p_value = p.value) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(estimate_actual)

# Wrangle statistics outputs
statistic_actual <- purrr::list_rbind(x = statistic_list) %>% 
  # Replace periods with underscores in column names
  dplyr::rename_with(.fn = ~ gsub(pattern = "\\.", replacement = "_", x = .x),
                     .cols = dplyr::contains(".")) %>% 
  # Rename ambiguously named columns
  dplyr::rename(F_statistic = statistic,
                test_p_value = p_value) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(statistic_actual)

## ----------------------------------------- ##
# Combine Outputs ----
## ----------------------------------------- ##

# Combine these three types of output
combo_v1 <- data_actual %>% 
  # Attach statistical information
  dplyr::left_join(y = statistic_actual,
                   by = c("sizer_bandwidth", "stream", "section")) %>% 
  # Attach estimate information too
  dplyr::left_join(y = estimate_actual,
                   by = c("sizer_bandwidth", "stream", "section"))

# Check structure
dplyr::glimpse(combo_v1)

# Process this to make navigating it more intuitive/easier
combo_v2 <- combo_v1 %>% 
  # Reorder 'site information' (i.e., grouping columns) columns to the left
  dplyr::relocate(sizer_bandwidth, LTER, stream, drainSqKm, chemical, 
                  Month:Year, dplyr::contains(response),
                  section, dplyr::starts_with("section_"),
                  .before = dplyr::everything()) %>% 
  # Rename columns as needed
  dplyr::rename(sizer_slope = slope_type) %>% 
  # Fix column class issues
  dplyr::mutate(sizer_bandwidth = as.numeric(sizer_bandwidth),
                Year = as.numeric(Year)) %>%
  dplyr::mutate(dplyr::across(.cols = r_squared:std_error, .fns = as.numeric)) %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(response), .fns = as.numeric)) %>%
  # Reorder statistical columns more informatively
  dplyr::relocate(F_statistic, test_p_value, r_squared, adj_r_squared, sigma, 
                  df, df_residual, nobs, logLik, AIC, BIC, deviance, 
                  .after = sizer_slope) %>%
  dplyr::relocate(estimate, std_error, term_statistic, term_p_value, 
                  .after = dplyr::everything()) %>%
  # Rename slope estimate column more clearly
  dplyr::rename(slope_estimate = estimate,
                slope_std_error = std_error)

# Check structure
dplyr::glimpse(combo_v2)

# Create some other desired columns
combo_v3 <- combo_v2 %>%
  # Simplify river names slightly
  dplyr::mutate(site_simp = gsub(pattern = " at", replacement = " ", x = stream)) %>% 
  # Simplify other place information column contents
  dplyr::mutate(LTER_simp = ifelse(nchar(LTER) <= 4, yes = LTER,
                                     no = stringr::str_sub(LTER, start = 1, end = 4)),
                site_simp = ifelse(nchar(site_simp) <= 14, yes = site_simp,
                                     no = stringr::str_sub(site_simp, start = 1, end = 14)),
                LTER_stream = paste0(LTER_simp, "_", site_simp), .after = stream) %>% 
  # Drop intermediary columns
  dplyr::select(-dplyr::ends_with("_simp")) %>% 
  # Calculate average 'response' per SiZer section
  dplyr::group_by(sizer_bandwidth, stream, chemical, section) %>%
  dplyr::mutate(mean_response = mean(.data[[response]], na.rm = T),
                sd_response = sd(.data[[response]], na.rm = T),
                .before = section) %>%
  dplyr::ungroup() %>% 
  # Express slope as a percent change of average response
  dplyr::mutate(percent_change = (slope_estimate / mean_response) * 100,
                .after = sd_response)

# Make sure the new 'LTER_stream' column is as unique as raw LTER + stream
length(unique(paste0(combo_v3$LTER, combo_v3$stream)))
length(unique(combo_v3$LTER_stream))

# Check structure
dplyr::glimpse(combo_v3)

## ----------------------------------------- ##
                # Export ----
## ----------------------------------------- ##

# Export that combination object locally
## Can use special folder name as *file name* to ensure informative naming conventions
write.csv(x = combo_v3, na = "", row.names = F,
          file = file.path("data", paste0("sizer-outs_", output_dir, ".csv")))

# End ----
