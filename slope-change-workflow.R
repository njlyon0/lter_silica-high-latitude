## ------------------------------------------------------- ##
          # `SiZer` Workflow for Silica Export WG
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

## ----------------------------------------- ##
            # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(broom, cowplot, SiZer, tidyverse, lter/HERON)

# Clear environment
rm(list = ls())

# Load data
data_v0 <- readr::read_csv(file = file.path("data", "Full_Results_ResultsTable_GFN_WRTDS.csv"))

# Now subset to sites of interest
data <- data_v0 %>%
  # Keep only polar sites
  dplyr::filter(LTER %in% c("MCM", "ARC", "GRO", "Finnish Environmental Institute","NIVA") | stream %in% c ("Site 7")) %>%
  # But drop one site that is technically polar
  dplyr::filter(stream != "Site 69038")

# Take a look!
dplyr::glimpse(data)

## ----------------------------------------- ##
          # Pre-Loop Preparation ----
## ----------------------------------------- ##

# Identify response (Y) and explanatory (X) variables
response_var <- "FNYield"
explanatory_var <- "Year"

# Identify which chemical you want to analyze
element <- "DSi"

# Do a quick typo check
if(!response_var %in% names(data)) {
  message("Response not found in data! Check spelling.") } else {
    message("Response variable looks good!") }
if(!explanatory_var %in% names(data)) {
  message("Explanatory not found in data! Check spelling.") } else {
    message("Explanatory variable looks good!") }
if(!element %in% data$chemical) {
  message("Chemical not found in data! Check spelling.") } else {
    message("Chemical looks good!") }

# Identify the three bandwidths you want to look at specifically
band_low <- 4
band_mid <- 5
band_high <- 8

# Create a folder to save experimental outputs
# Folder name is: [response]_bw[bandwidths]_[date]
(export_folder <- paste0("export_", response_var, "_",
                         element, "_bw",
                         band_low, band_mid, band_high,
                         "_", Sys.Date()))
dir.create(path = export_folder, showWarnings = FALSE)

# Make an empty list to store all of our extracted information
giant_list <- list()

# Make a counter and set it to 1 (the list will add to it)
j <- 1

## ----------------------------------------- ##
       # Loop Extract of SiZer Data ----
## ----------------------------------------- ##

# Loop through sites and extract information
# for(place in unique(data$stream)) {
for(place in "Site 7"){
  
  # Start with a message!
  message("Processing begun for '", response_var, "' of '", chemical, "' at '", place, "'")
  
  # Subset the data
  data_sub <- data %>%
    dplyr::filter(stream == place) %>%
    dplyr::filter(chemical == element) %>%
    as.data.frame()
  
  # Loop - Get SiZer Object ----
  # Print a progress message
  message("Run SiZer...")
  
  # Invoke the SiZer::SiZer function
  e <- SiZer::SiZer(x = data_sub[[explanatory_var]],
                    y = data_sub[[response_var]],
                    h = c(2, 10), degree = 1,
                    derv = 1, grid.length = 100)
  
  # Make a shorter place name
  place_short <- stringr::str_sub(string = place, start = 1, end = 8)
  
  # Plot (and export) the SiZer object with horizontal lines of interest
  png(filename = file.path(export_folder, paste0(place_short, "_SiZer-plot.png")), width = 5, height = 5, res = 720, units = 'in')
  HERON::sizer_plot(sizer_object = e,
                    bandwidth_vec = c(band_low, band_mid, band_high))
  dev.off()
  
  # Strip out the aggregated (across all bandwidths) inflection points
  sizer_tidy <- HERON::sizer_aggregate(sizer_object = e)
  
  # Identify inflection points at three specific bandwidths too
  sizer_low <- HERON::sizer_slice(sizer_object = e, bandwidth = band_low)
  sizer_mid <- HERON::sizer_slice(sizer_object = e, bandwidth = band_mid)
  sizer_high <- HERON::sizer_slice(sizer_object = e, bandwidth = band_high)
  
  # Loop - Identify Slope Changes ----
  # Print a progress message
  message("Find slope changes...")
  
  # Identify inflection points
  ## Aggregate
  data_sub_agg <- HERON::id_slope_changes(raw_data = data_sub,
                                   sizer_data = sizer_tidy,
                                   x = explanatory_var,
                                   y = response_var)
  ## Low
  data_sub_low <- HERON::id_slope_changes(raw_data = data_sub,
                                   sizer_data = sizer_low,
                                   x = explanatory_var,
                                   y = response_var)
  ## Mid
  data_sub_mid <- HERON::id_slope_changes(raw_data = data_sub,
                                   sizer_data = sizer_mid,
                                   x = explanatory_var,
                                   y = response_var)
  ## High
  data_sub_high <- HERON::id_slope_changes(raw_data = data_sub,
                                    sizer_data = sizer_high,
                                    x = explanatory_var,
                                    y = response_var)
  
  # Loop - Make Plots ----
  # Print a progress message
  message("Making plots...")
  
  # Plot the aggregated inflection points
  agg_plot <- HERON::sizer_ggplot(raw_data = data_sub_agg,
                           sizer_data = sizer_tidy,
                           x = explanatory_var, y = response_var,
                           trendline = 'sharp', vline = "none",
                           sharp_colors = c("#bbbbbb", 'orange')) +
    ggtitle(label = "Aggregated Slope Changes")
  
  # Plot the bandwidth-specific plots too!
  ## Low Bandwidth (h)
  low_plot <- HERON::sizer_ggplot(raw_data = data_sub_low,
                           sizer_data = sizer_low,
               x = explanatory_var, y = response_var,
               trendline = 'sharp', vline = "none",
               sharp_colors = c("#bbbbbb", 'orange')) +
    ggtitle(label = paste0("h = ", band_low, " Slope Changes"))
  ## Mid Bandwidth (h)
  mid_plot <- HERON::sizer_ggplot(raw_data = data_sub_mid,
                           sizer_data = sizer_mid,
               x = explanatory_var, y = response_var,
               trendline = 'sharp', vline = "none",
               sharp_colors = c("#bbbbbb", 'orange')) +
    ggtitle(label = paste0("h = ", band_mid, " Slope Changes"))
  ## High Bandwidth (h)
  high_plot <- HERON::sizer_ggplot(raw_data = data_sub_high,
                            sizer_data = sizer_high,
               x = explanatory_var, y = response_var,
               trendline = 'sharp', vline = "none",
               sharp_colors = c("#bbbbbb", 'orange')) +
    ggtitle(label = paste0("h = ", band_high, " Slope Changes"))
  
  # Combine plots
  combo_plot <- cowplot::plot_grid(agg_plot, low_plot,
                                   mid_plot, high_plot,
                                   nrow = 2, ncol = 2,
                                   labels = "AUTO")
  
  ggplot2::ggsave(plot = combo_plot, height = 8, width = 8,
                  filename = file.path(export_folder, paste0("slope-change_", place_short, "_ggplots.png")))
  
  # Loop - Wrangle SiZer Data ----
  # Print a progress message
  message("Wrangling SiZer data...")
  
  # Now modify the columns in the provided sizer dataframes
  sizer_tidy_export <- sizer_tidy %>%
    # Make everything a character
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    # Add a column for bandwidth and for site name
    dplyr::mutate(site = place, h_grid = "averaged across bandwidths",
                  .before = dplyr::everything() ) %>%
    # Make sure it's a dataframe
    as.data.frame()
  
  # Do the same for the bandwidth specific data
  ## Low Bandwidth
  sizer_low_export <- sizer_low %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(site = place, .before = dplyr::everything()) %>%
    as.data.frame()
  ## Mid Bandwidth
  sizer_mid_export <- sizer_mid %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(site = place, .before = dplyr::everything()) %>%
    as.data.frame()
  ## High Bandwidth
  sizer_high_export <- sizer_high %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(site = place, .before = dplyr::everything()) %>%
    as.data.frame()
  
  # Combine the specific bandwidth objects
  complete_export <- dplyr::bind_rows(sizer_low_export,
                                      sizer_mid_export,
                                      sizer_high_export)
  
  # Add these tidied dataframes to our lists
  giant_list[[paste0("aggregate_", j)]] <- sizer_tidy_export
  giant_list[[paste0("specific_", j)]] <- complete_export

  # Loop - Fit Linear Models ----
  # Print a progress message
  message("Fit regressions...")
  
  # Extract (1) statistics and (2) estimates from linear models
  agg_lm <- HERON::sizer_lm(data = data_sub_agg, x = explanatory_var,
                     y = response_var, group_col = "groups") %>%
    # Then add columns for which bandwidth & which site
    purrr::map(.f = mutate, bandwidth = "aggregate",
               .before = dplyr::everything())
  ## Low bandwidth
  low_lm <- HERON::sizer_lm(data = data_sub_low, x = explanatory_var,
                     y = response_var, group_col = "groups") %>%
    purrr::map(.f = mutate, bandwidth = band_low,
               .before = dplyr::everything())
  ## Middle bandwidth
  mid_lm <- HERON::sizer_lm(data = data_sub_mid, x = explanatory_var,
                     y = response_var, group_col = "groups") %>%
    purrr::map(.f = mutate, bandwidth = band_mid,
               .before = dplyr::everything())
  ## High bandwidth
  high_lm <- HERON::sizer_lm(data = data_sub_high, x = explanatory_var,
                      y = response_var, group_col = "groups") %>%
    purrr::map(.f = mutate, bandwidth = band_high,
               .before = dplyr::everything())
  
  # Form one big list
  mega_lm_list <- list(agg_lm, low_lm, mid_lm, high_lm)

  # Final dataframe processing for *statistics*
  stat_df <- mega_lm_list %>%
    # Extract first list element
    purrr::map(.f = 1) %>%
    # Make all columns characters
    purrr::map(.f = dplyr::mutate, dplyr::across(dplyr::everything(),
                                          as.character)) %>%
    # Add a site column
    purrr::map(.f = mutate, site = place,
               .before = dplyr::everything()) %>%
    # Combine all list elements into a dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything())
  
  # Final dataframe processing for *estimates*
 est_df <- mega_lm_list %>%
    purrr::map(.f = 2) %>%
    purrr::map(.f = dplyr::mutate, dplyr::across(dplyr::everything(),
                                                 as.character)) %>%
    purrr::map(.f = mutate, site = place,
               .before = dplyr::everything()) %>%
    purrr::map_dfr(.f = dplyr::select, dplyr::everything())
  
  # Add this information to their respective lists
  giant_list[[paste0("stats_", j)]] <- stat_df
  giant_list[[paste0("estimates_", j)]] <- est_df
  
  # Increase the counter by 1 (for the next iteration of the loop)
  j <- j + 1
  
  # Return a "finished" message!
  message("Processing complete for '", response_var, "' of '", chemical, "' at '", place, "'")
  }

## ----------------------------------------- ##
    # Unlist and Export Looped Data ----
## ----------------------------------------- ##

# Check out what is in our huge list
names(giant_list)

# Now (ironically) we'll use a loop to unlist what the first loop made
for(data_type in c("aggregate", "specific", "stats", "estimates")){
  
  # For each data type...
  list_sub <- giant_list %>%
    # Identify all list elements that contain this type of data
    purrr::keep(.p = stringr::str_detect(string = names(.),
                                         pattern = data_type)) %>%
    # Unlist by selecting all columns of each list element
    purrr::map_dfr(.f = dplyr::select, dplyr::everything())
  
  # Now save the CSV
  write_csv(x = list_sub, na = "",
            file = file.path(export_folder,
                             paste0("_slope-change_", data_type, "_exported.csv")))
  
  # And print a message
  message("Dataframe for ", data_type, " exported.") }

# End ----
