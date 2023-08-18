## ------------------------------------------------------- ##
          # `SiZer` Workflow for Silica Export WG
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Run SiZer workflow on *annual* data produced by WRTDS
## WRTDS = Weighted Regressions on Time, Discharge, and Season

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Need to force an install of HERON to get an updated version?
# devtools::install_github("lter/HERON", force = T)

# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, SiZer, tidyverse, lter/HERON, supportR)

# Clear environment
rm(list = ls())

# Identify files in Drive folder
ids <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1V5EqmOlWA8U9NWfiBcWdqEH9aRAP-zCk")) %>%
  # Filter to desired data files
  dplyr::filter(name %in% c("Full_Results_ResultsTable_GFN_WRTDS.csv"))

# Check that includes all desired data files
ids

# Create a folder to save to
dir.create(path = "data", showWarnings = F)

# Download desired data
purrr::walk2(.x = ids$id, .y = ids$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("data", .y),
                                                overwrite = T))

# Load data
data_v0 <- readr::read_csv(file = file.path("data", "Full_Results_ResultsTable_GFN_WRTDS.csv"))

# Now subset to sites of interest
data_simp <- data_v0 %>%
  # Keep only cryosphere LTERs
  dplyr::filter(LTER %in% c("MCM", "ARC", "GRO", "NIVA", "Krycklan",
                            "Finnish Environmental Institute")) %>%
  # But drop problem sites that are otherwise retained
  dplyr::filter(!stream %in% c("Site 69038", "Kymijoki Ahvenkoski 001",
                               "Kymijoki Kokonkoski 014")) %>%
  # Calculate number of years
  dplyr::group_by(LTER, stream) %>%
  dplyr::mutate(num_years = length(unique(Year)), .after = Year) %>%
  dplyr::ungroup() %>%
  # Filter to only more than some threshold years
  dplyr::filter(num_years >= 15) %>%
  # Drop that column now that we've used it
  dplyr::select(-num_years) %>%
  # Convert 10^-6 xx to just xx
  dplyr::mutate(Flux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                    yes = (Flux_10_6kg_yr * 10^6),
                                    no = NA),
                FNFlux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (FNFlux_10_6kg_yr * 10^6),
                                      no = NA),
                Flux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (Flux_10_6kmol_yr * 10^6),
                                      no = NA),
                FNFlux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                        yes = (FNFlux_10_6kmol_yr * 10^6),
                                        no = NA),
                Yield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                           yes = (Yield_10_6kmol_yr_km2 * 10^6),
                                           no = NA),
                FNYield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                             yes = (FNYield_10_6kmol_yr_km2 * 10^6),
                                             no = NA)) %>%
  # Tweak chemical names to exclude `:` in ratio
  dplyr::mutate(chemical = dplyr::case_when(
    chemical == "Si:DIN" ~ "Si_DIN",
    chemical == "Si:P" ~ "Si_P",
    TRUE ~ chemical))
  
# Take a look!
dplyr::glimpse(data_simp)

# Take a look at LTERs / streams
data_simp %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = length(unique(stream)))

# Check lost/gained columns
supportR::diff_check(old = names(data_v0), new = names(data_simp))

## ----------------------------------------- ##
          # Pre-Loop Preparation ----
## ----------------------------------------- ##

# Identify response (Y) and explanatory (X) variables
response_var <- "Yield_kmol_yr_km2"
## Yield, Conc_uM, Discharge_cms
explanatory_var <- "Year"

# Identify which chemical you want to analyze
element <- "DSi"
unique(data_simp$chemical)

# Do a quick typo check
if(!response_var %in% names(data_simp)) {
  message("Response not found in data! Check spelling.") } else {
    message("Response variable looks good!") }
if(!explanatory_var %in% names(data_simp)) {
  message("Explanatory not found in data! Check spelling.") } else {
    message("Explanatory variable looks good!") }
if(!element %in% data_simp$chemical) {
  message("Chemical not found in data! Check spelling.") } else {
    message("Chemical looks good!") }

# Identify the bandwidth to use with SiZer
bandwidth <- 5

# Carve data down to only needed bits
data_short <- data_simp %>%
  ## Keep only needed columns
  dplyr::select(LTER:chemical, dplyr::starts_with(response_var)) %>%
  ## And only chemical of interest
  dplyr::filter(chemical == element)

# Check that out
dplyr::glimpse(data_short)

# Make sure there are some non-NAs for the selected response/explanatory/chemical
if(all(is.na(data_short[[response_var]])) == T){
  message("No non-NA response values found in this subset of the data")
} else { message("Data subset looks good!") }

# Create a folder to save experimental outputs
# Folder name is: [response]_bw[bandwidths]_[date]
(export_folder <- paste0("annual_", response_var, "_", element, "_bw", bandwidth))
dir.create(path = export_folder, showWarnings = FALSE)

# Make an empty list to store all of our extracted information
giant_list <- list()

# Make a counter and set it to 1 (the list will add to it)
j <- 1

## ----------------------------------------- ##
           # Extract SiZer Data ----
## ----------------------------------------- ##

# Loop through sites and extract information
for(place in unique(data_short$stream)) {
  # for(place in c("Yukon", "Site 7")) {
  
  # Start with a message!
  message("Processing begun for '", response_var, "' of '", element, "' at '", place, "'")
  
  # Subset the data
  data_sub <- data_short %>%
    dplyr::filter(stream == place) %>%
    as.data.frame()
  
  # Loop - Get SiZer Object ----
  message("Run SiZer...")
  
  # Invoke the SiZer::SiZer function
  e <- SiZer::SiZer(x = data_sub[[explanatory_var]],
                    y = data_sub[[response_var]],
                    h = c(2, 10), degree = 1,
                    derv = 1, grid.length = 100)
  
  # Make a shorter place name
  place_short <- stringr::str_sub(string = place, start = 1, end = 8)
  
  # Plot (and export) the SiZer object with horizontal lines of interest
  png(filename = file.path(export_folder, paste0(place_short, "_SiZer-plot.png")),
      width = 5, height = 5, res = 720, units = 'in')
  HERON::sizer_plot(sizer_object = e,
                    bandwidth_vec = c(bandwidth))
  dev.off()
  
  # Identify inflection points/slope changes
  sizer_info <- HERON::sizer_slice(sizer_object = e, bandwidth = bandwidth)
  
  # Loop - No Changes Workflow ----
  ## If no slope changes are found:
  if(nrow(sizer_info) == 0){
    
    # Message this status
    message("No slope changes/inflections found; Proceeding...")
    
    # Migrate "groups" over 
    data_info <- HERON::id_slope_changes(raw_data = data_sub,
                                         sizer_data = sizer_info,
                                         x = explanatory_var,
                                         y = response_var,
                                         group_dig = 5)
    
    # Make plot
    demo_plot <- HERON::sizer_ggplot(raw_data = data_info,
                                     sizer_data = sizer_info,
                                     x = explanatory_var, y = response_var,
                                     trendline = 'sharp', vline = "none") +
      ggtitle(label = paste0("h = ", bandwidth, " Slope Changes (None)"))
    
  } else {
    
    # If there are changes and/or inflections, find inflections
    inflects_raw <- c(sizer_info$neg_to_pos, sizer_info$pos_to_neg)
    inflects <- inflects_raw[!is.na(inflects_raw)]
    
    # Loop - Inflection Point Workflow ----
    # If any inflections *are* found:
    if(length(inflects) > 0){
      
      # Message to this effect
      message("Inflections found; Proceeding...")
      
      # Migrate groups over
      data_info <- HERON::id_inflections(raw_data = data_sub,
                                         sizer_data = sizer_info,
                                         x = explanatory_var,
                                         y = response_var,
                                         group_dig = 5)
      
      # Make plot
      demo_plot <- HERON::sizer_ggplot(raw_data = data_info,
                                       sizer_data = sizer_info,
                                       x = explanatory_var, y = response_var,
                                       trendline = 'sharp', vline = "inflections",
                                       sharp_colors = c("#bbbbbb", "green")) +
        ggtitle(label = paste0("h = ", bandwidth, " Inflection Points"))
      
    } else {
      # Loop - Slope Change Workflow ----
      
      # Message
      message("Slope changes found but no inflections; Proceeding...")
      
      # Strip group assignments
      data_info <- HERON::id_slope_changes(raw_data = data_sub, sizer_data = sizer_info,
                                           x = explanatory_var, y = response_var,
                                           group_dig = 5)
      
      # Plot 
      demo_plot <- HERON::sizer_ggplot(raw_data = data_info,
                                       sizer_data = sizer_info,
                                       x = explanatory_var, y = response_var,
                                       trendline = 'sharp', vline = "changes",
                                       sharp_colors = c("#bbbbbb", "green")) +
        ggtitle(label = paste0("h = ", bandwidth, " Slope Changes"))
    } } # Close tri-partite workflow splits
  
  # Export whichever graph got made
  ggplot2::ggsave(filename = file.path(export_folder, 
                                       paste0(place_short, "_ggplot.png")),
                  height = 8, width = 8)
  
  # Loop - Wrangle SiZer Data ----
  message("Wrangling SiZer data...")
  
  # Modify the columns in the provided sizer dataframes
  sizer_export <- data_info %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(bandwidth_h = bandwidth,
                  site = place, 
                  .before = dplyr::everything()) %>%
    as.data.frame()
  
  # Add this tidied dataframe to our export list
  giant_list[[paste0("data_", j)]] <- sizer_export
  
  # Loop - Fit Linear Models ----
  message("Fit regressions...")
  
  # Extract statistics/estimates from linear models
  lm_obj <- HERON::sizer_lm(data = data_info, x = explanatory_var,
                            y = response_var, group_col = "groups") %>%
    # Then add column for bandwidth
    purrr::map(.f = mutate, bandwidth_h = bandwidth,
               .before = dplyr::everything())
  
  # Final dataframe processing for *statistics*
  stat_df <- lm_obj[[1]] %>%
    # Make all columns characters
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    # Add a site column
    dplyr::mutate(site = place, .before = dplyr::everything())
  
  # Final dataframe processing for *estimates*
  est_df <- lm_obj[[2]] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(site = place, .before = dplyr::everything())
  
  # Add this information to their respective lists
  giant_list[[paste0("stats_", j)]] <- stat_df
  giant_list[[paste0("estimates_", j)]] <- est_df
  
  # Increase the counter by 1 (for the next iteration of the loop)
  j <- j + 1
  
  # Return a "finished" message!
  message("Processing complete for '", response_var, "' of '", element, "' at '", place, "'")
  
} # Close loop

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
  # Drop 'stream' and 'groups' columns (redundant with 'site', and 'section' respectively)
  dplyr::select(-stream, -groups) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(years_v2)

# Combine these data files
combo_v1 <- years_v2 %>%
  # Attach statistical information to response data
  dplyr::left_join(y = stats_v2, by = dplyr::join_by(bandwidth_h, site, section)) %>%
  # Attach estimate information to response data
  dplyr::left_join(est_v2, by = join_by(bandwidth_h, site, section))

# Check structure
dplyr::glimpse(combo_v1)
## view(combo_v1)

# Let's process this to be a little friendlier for later use
combo_v2 <- combo_v1 %>%
  # Reorder 'site information' (i.e., grouping columns) columns to the left
  dplyr::relocate(bandwidth_h, LTER, site, drainSqKm, season, chemical, 
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
  dplyr::mutate(site = gsub(pattern = " at", replacement = " ", x = site)) %>%
  # Create a column that combines LTER and stream names
  dplyr::mutate(LTER_abbrev = ifelse(nchar(LTER) > 4,
                                     yes = stringr::str_sub(string = LTER, start = 1, end = 4),
                                     no = LTER),
                site_abbrev = ifelse(nchar(site) > 14,
                                     yes = stringr::str_sub(string = site, start = 1, end = 14),
                                     no = site),
                stream = paste0(LTER_abbrev, "_", site_abbrev), .after = site) %>%
  # Drop intermediary columns needed to make that abbreviation simply
  dplyr::select(-dplyr::ends_with("_abbrev")) %>%
  # Calculate relative response so sites with very different absolute totals can be directly compared
  ## Calculate average 'response' per SiZer section
  dplyr::group_by(sizer_bandwidth, stream, season, chemical, section) %>%
  dplyr::mutate(mean_response = mean(.data[[response_var]], na.rm = T),
                sd_response = sd(.data[[response_var]], na.rm = T),
                .before = section) %>%
  dplyr::ungroup() %>%
  # Divide average by slope estimate and convert into percent change
  dplyr::mutate(percent_change = (mean_response / slope_estimate) * 100,
                .after = sd_response)

# Make sure the new 'stream' column is as unique as raw LTER + stream
length(unique(paste0(combo_v3$LTER, combo_v3$site)))
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
write.csv(x = combo_v2, na = "", row.names = F,
          file = file.path("sizer_outs", paste0(export_folder, ".csv")))

# End ----
