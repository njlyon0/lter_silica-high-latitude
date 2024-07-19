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

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, SiZer, supportR, lter/HERON)

# Clear environment
rm(list = ls())

# Load custom functions
for(fxn in dir(path = file.path("tools"))){
  source(file.path("tools", fxn))
}

## And remove loop index object from environment
rm(list = "fxn")

## ----------------------------------------- ##
# General SiZer Prep ----
## ----------------------------------------- ##

# Read in data & rename a column
wrtds_v1 <- read.csv(file = file.path("data", "Full_Results_WRTDS_annual.csv"))

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
temporal_res <- "annual"

# Choose response/explanatory variables of interest & focal chemical
response <- "FNConc_uM"
explanatory <- "Year"
element <- "DIN"

# Check that combination of variables works
var_check(data = wrtds_v4, chem = element, 
          resp_var = response, exp_var = explanatory)

# Prepare just the desired pieces of information
wrtds_focal <- wrtds_v4 %>% 
  dplyr::select(LTER:chemical, dplyr::contains(response)) %>% 
  dplyr::filter(chemical == element)

# Check structure
dplyr::glimpse(wrtds_focal)

# Create a folder for outputs
(output_dir = paste(temporal_res, response, element, sep = "_"))
dir.create(path = file.path(output_dir), showWarnings = F)

## ----------------------------------------- ##
          # Core SiZer Workflow ----
## ----------------------------------------- ##

# Make an empty list to store all of our extracted information
giant_list <- list()

# Make a counter and set it to 1 (the loop will add to it)
j <- 1

# Loop across streams
# for(place in unique(wrtds_focal$stream)){
for(place in "Iijoki Raasakan voimal"){
  
  # Starting message
  message("Processing begun for ", element, " ", response, " at ", place)
  
  # Make a shorter place name
  place_short <- stringr::str_sub(string = place, start = 1, end = 8)
  
  # Subset to just this stream
  wrtds_place <- dplyr::filter(.data = wrtds_focal, stream == place)
  
  # Message for starting of SiZer bit
  message("Run SiZer...")
  
  # Invoke the SiZer::SiZer function
  sizer_obj <- SiZer::SiZer(x = wrtds_place[[explanatory]], y = wrtds_place[[response]],
                            h = c(2, 10), degree = 1, derv = 1, grid.length = 100)
  
  # Plot (and export) the SiZer object with horizontal lines of interest
  png(filename = file.path(output_dir, paste0(place_short, "_SiZer-plot.png")),
      width = 5, height = 5, res = 720, units = 'in')
  HERON::sizer_plot(sizer_object = sizer_obj, bandwidth_vec = 5)
  dev.off()
  
  # Identify inflection points/slope changes
  sizer_info <- HERON::sizer_slice(sizer_object = sizer_obj, bandwidth = 5)
  
  # If there are changes and/or inflections, find inflections
  inflects_raw <- c(sizer_info$neg_to_pos, sizer_info$pos_to_neg)
  inflects <- inflects_raw[!is.na(inflects_raw)]
  
  ## If no slope changes are found:
  if(nrow(sizer_info) == 0){
    
    # Message this status
    message("No slope changes/inflections found; Proceeding...")
    
    # Migrate "groups" over 
    place_info <- HERON::id_slope_changes(raw_data = wrtds_place, sizer_data = sizer_info,
                                          x = explanatory, y = response)
    
    # Make plot
    demo_plot <- HERON::sizer_ggplot(raw_data = place_info, sizer_data = sizer_info,
                                     x = explanatory, y = response,
                                     trendline = 'sharp', vline = "none") +
      ggtitle(label = paste0("h = 5 Slope Changes (None)"))
    
    ## If inflection points (slope sign changes) are found:
  } else if(length(inflects) > 0){ 
    
    # Message this status
    message("Inflections found; Proceeding...")
    
    # Migrate groups over
    place_info <- HERON::id_inflections(raw_data = wrtds_place, sizer_data = sizer_info,
                                        x = explanatory, y = response)
    
    # Make plot
    demo_plot <- HERON::sizer_ggplot(raw_data = place_info, sizer_data = sizer_info,
                                     x = explanatory, y = response,
                                     trendline = 'sharp', vline = "inflections",
                                     sharp_colors = c("#bbbbbb", "green")) +
      ggtitle(label = paste0("h = 5 Inflection Points"))
    
    ## If slope changes (but not inflection points) are found:
  } else {
    
    # Message this status
    message("Slope changes found but no inflections; Proceeding...")
    
    # Strip group assignments
    place_info <- HERON::id_slope_changes(raw_data = wrtds_place, sizer_data = sizer_info,
                                          x = explanatory, y = response)
    
    # Plot 
    demo_plot <- HERON::sizer_ggplot(raw_data = place_info, sizer_data = sizer_info,
                                     x = explanatory, y = response,
                                     trendline = 'sharp', vline = "changes",
                                     sharp_colors = c("#bbbbbb", "green")) +
      ggtitle(label = paste0("h = 5 Slope Changes"))
  }
  
  # Export whichever graph got made
  ggplot2::ggsave(filename = file.path(output_dir, paste0(place_short, "_ggplot.png")),
                  height = 8, width = 8)
  
  # Wrangle SiZer output for export
  message("Wrangling SiZer data...")
  
  # Modify the columns in the provided sizer dataframes
  place_export <- place_info %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    dplyr::mutate(bandwidth_h = 5,
                  stream = place, 
                  .before = dplyr::everything())
  
  # Add this tidied dataframe to our export list
  giant_list[[paste0("data_", j)]] <- place_export
  
  # Fit linear models
  message("Fit regressions...")
  
  # Extract statistics/estimates from linear models
  lm_obj <- HERON::sizer_lm(data = place_info, x = explanatory,
                            y = response, group_col = "groups") %>%
    # Then add column for bandwidth
    purrr::map(.f = mutate, bandwidth_h = 5,
               .before = dplyr::everything())
  
  # Final dataframe processing for *statistics*
  stat_df <- lm_obj[[1]] %>%
    # Make all columns characters
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    # Add a site column
    dplyr::mutate(stream = place, .before = dplyr::everything())
  
  # Final dataframe processing for *estimates*
  est_df <- lm_obj[[2]] %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    dplyr::mutate(stream = place, .before = dplyr::everything())
  
  # Add this information to their respective lists
  giant_list[[paste0("stats_", j)]] <- stat_df
  giant_list[[paste0("estimates_", j)]] <- est_df
  
  # Increase the counter by 1 (for the next iteration of the loop)
  j <- j + 1
  
} # Close loop

  
  
  
  
  
  
  
  
  
  






# Basement ----




## ----------------------------------------- ##
# Process Loop Outputs ----
## ----------------------------------------- ##

# Check out what is in our huge list
names(giant_list)

# Make a new list to store simplified outputs in
result_list <- list()

# Now (ironically) we'll use a loop to unlist what the first loop made
for(data_type in c("data", "stats", "estimates")){
  
  # For each data type...
  list_sub <- giant_list %>%
    # Identify all list elements that contain this type of data
    purrr::keep(.p = stringr::str_detect(string = names(.),
                                         pattern = data_type)) %>%
    # Unlist by selecting all columns of each list element
    purrr::list_rbind()
  
  # Add this to the simpler results list
  result_list[[data_type]] <- list_sub
  
  # And print a message
  message("Dataframe for ", data_type, " extracted") }

# Check out the simplified results list we're left with
names(result_list)
dplyr::glimpse(result_list)

# Grab each bit as a dataframe for ease of further modification
estimates <- result_list[["estimates"]]
stats <- result_list[["stats"]]
years <- result_list[["data"]]

# Wrangle estimate part
est_v2 <- estimates %>%
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
dplyr::glimpse(est_v2)

# Wrangle statistics part
stats_v2 <- stats %>%
  # Rename period columns to use underscores
  dplyr::rename(p_value = p.value,
                r_squared = r.squared,
                adj_r_squared = adj.r.squared,
                df_residual = df.residual) %>%
  # Rename ambiguously named columns
  dplyr::rename(F_statistic = statistic,
                test_p_value = p_value) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(stats_v2)

# Wrangle source data ('years')
years_v2 <- years %>%
  # Tweak the '-Inf to Inf' entry for consistency with other dataframes and rename column
  dplyr::mutate(section = ifelse(groups == "(-Inf, Inf]",
                                 yes = "No inflection points", no = groups), 
                .after = LTER) %>%
  # Make end/start actually numbers and calculate duration of group
  dplyr::mutate(start = as.numeric(start),
                end = as.numeric(end),
                duration = end - start,
                .after = end) %>%
  # Make bandwidth a character
  dplyr::mutate(bandwidth_h = as.character(bandwidth_h)) %>%
  # Drop 'stream' and 'groups' columns (redundant with 'Stream_Name', and 'section' respectively)
  dplyr::select(-stream, -groups) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(years_v2)

# Combine these data files
combo_v1 <- years_v2 %>%
  # Attach statistical information to response data
  dplyr::left_join(y = stats_v2, by = dplyr::join_by(bandwidth_h, Stream_Name, section)) %>%
  # Attach estimate information to response data
  dplyr::left_join(est_v2, by = join_by(bandwidth_h, Stream_Name, section))

# Check structure
dplyr::glimpse(combo_v1)
## view(combo_v1)

# Let's process this to be a little friendlier for later use
combo_v2 <- combo_v1 %>%
  # Reorder 'site information' (i.e., grouping columns) columns to the left
  dplyr::relocate(bandwidth_h, LTER, Stream_Name, drainSqKm, chemical, 
                  Year, dplyr::contains(response_var),
                  section, start, end, duration, .before = dplyr::everything()) %>%
  # Rename columns as needed
  dplyr::rename(sizer_bandwidth = bandwidth_h,
                section_start = start,
                section_end = end,
                section_duration = duration,
                sizer_slope = slope_type) %>%
  # Fix column class issues
  dplyr::mutate(sizer_bandwidth = as.numeric(sizer_bandwidth),
                Year = as.numeric(Year)) %>%
  dplyr::mutate(dplyr::across(.cols = r_squared:std_error, .fns = as.numeric)) %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::contains(response_var), .fns = as.numeric)) %>%
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

# Calculate / create some other desired columns
combo_v3 <- combo_v2 %>%
  # Simplify river names slightly
  dplyr::mutate(site_simp = gsub(pattern = " at", replacement = " ", x = Stream_Name)) %>%
  # Create a column that combines LTER and stream names
  dplyr::mutate(LTER_abbrev = ifelse(nchar(LTER) > 4,
                                     yes = stringr::str_sub(string = LTER, start = 1, end = 4),
                                     no = LTER),
                site_abbrev = ifelse(nchar(site_simp) > 14,
                                     yes = stringr::str_sub(string = site_simp, start = 1, end = 14),
                                     no = site_simp),
                stream = paste0(LTER_abbrev, "_", site_abbrev), .after = Stream_Name) %>%
  # Drop intermediary columns needed to make that abbreviation simply
  dplyr::select(-dplyr::ends_with("_abbrev"), -site_simp) %>%
  # Calculate relative response so sites with very different absolute totals can be directly compared
  ## Calculate average 'response' per SiZer section
  dplyr::group_by(sizer_bandwidth, stream, chemical, section) %>%
  dplyr::mutate(mean_response = mean(.data[[response_var]], na.rm = T),
                sd_response = sd(.data[[response_var]], na.rm = T),
                .before = section) %>%
  dplyr::ungroup() %>%
  # Express slope as a percent change of average response
  dplyr::mutate(percent_change = (slope_estimate / mean_response) * 100,
                .after = sd_response)

# Make sure the new 'stream' column is as unique as raw LTER + stream
length(unique(paste0(combo_v3$LTER, combo_v3$Stream_Name)))
length(unique(combo_v3$stream))

# Check structure yet again
dplyr::glimpse(combo_v3)
## view(combo_v3)

## ----------------------------------------- ##
# Export ----
## ----------------------------------------- ##

# Create folder to export this type of output too
dir.create(path = file.path("sizer_outs"), showWarnings = F)

# Export that combination object locally
## Can use special folder name as *file name* to ensure informative naming conventions
write.csv(x = combo_v3, na = "", row.names = F,
          file = file.path("sizer_outs", paste0(export_folder, ".csv")))

# End ----
