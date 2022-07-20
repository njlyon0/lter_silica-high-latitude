## ----------------------------------------- ##
   # `SiZer` Workflow for Silica Export WG
## ----------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Housekeeping ----

# Load libraries
# install.packages("librarian")
librarian::shelf(broom, cowplot, SiZer, tidyverse)

# Clear environment
rm(list = ls())

# Load data
data <- readr::read_csv(file = file.path("wg_silica", "CryoData_forNick_6.29.22.csv"))

# Load helper functions
source(file.path("wg_silica", "sizer-helper-fxns.R"))

# Loop Extract of SiZer Data ----

# Create a folder to save experimental outputs
export_folder <- "plots"
dir.create(path = export_folder, showWarnings = FALSE)

# Make an empty list to store all of our extracted information
giant_list <- list()

# Make a counter and set it to 1 (the list will add to it)
j <- 1

# Identify the three bandwidths you want to look at specifically
band_low <- 3
band_mid <- 6
band_high <- 9

# Loop through sites and extract information
for(place in unique(data$site)) {
# for(place in "ALBION"){
  
  # Start with a message!
  message("Processing begun for site: ", place)
  
  # Subset the data
  data_sub <- data %>%
    dplyr::filter(site == place) %>%
    as.data.frame()
  
  # Loop - ID Inflection Points ----
  # Print a progress message
  message("IDing inflection points...")
  
  # Invoke the SiZer::SiZer function
  e <- SiZer::SiZer(x = data_sub$Year, y = data_sub$FNYield,
                    h = c(2, 10), degree = 1,
                    derv = 1, grid.length = 100)
  
  # Make a shorter place name
  place_short <- stringr::str_sub(string = place, start = 1, end = 8)
  
  # Plot (and export) the SiZer object with horizontal lines of interest
  png(filename = file.path(export_folder, paste0(place_short, "_SiZer-plot.png")), width = 5, height = 5, res = 720, units = 'in')
  sizer_plot(sizer_object = e,
             bandwidth_vec = c(band_low, band_mid, band_high))
  dev.off()
  
  # Strip out the aggregated (across all bandwidths) inflection points
  sizer_tidy <- sizer_aggregate(sizer_object = e)
  
  # Identify inflection points at three specific bandwidths too
  sizer_low <- sizer_slice(sizer_object = e, bandwidth = band_low)
  sizer_mid <- sizer_slice(sizer_object = e, bandwidth = band_mid)
  sizer_high <- sizer_slice(sizer_object = e, bandwidth = band_high)
  
  # Loop - Make Plots ----
  # Print a progress message
  message("Making plots...")
  
  # Plot the aggregated inflection points
  agg_plot <- sizer_ggplot(raw_data = data_sub, sizer_data = sizer_tidy,
                           x = "Year", y = "FNYield") +
    ggtitle(label = "Aggregated Inflection Points")
  
  # Plot the bandwidth-specific plots too!
  ## Low Bandwidth (h)
  low_plot <- sizer_ggplot(raw_data = data_sub, sizer_data = sizer_low,
               x = "Year", y = "FNYield") +
    ggtitle(label = paste0("h = ", band_low, " Inflection Points"))
  ## Mid Bandwidth (h)
  mid_plot <- sizer_ggplot(raw_data = data_sub, sizer_data = sizer_mid,
               x = "Year", y = "FNYield") +
    ggtitle(label = paste0("h = ", band_mid, " Inflection Points"))
  ## High Bandwidth (h)
  high_plot <- sizer_ggplot(raw_data = data_sub, sizer_data = sizer_high,
               x = "Year", y = "FNYield") +
    ggtitle(label = paste0("h = ", band_high, " Inflection Points"))
  
  # Combine plots
  combo_plot <- cowplot::plot_grid(agg_plot, low_plot,
                                   mid_plot, high_plot,
                                   nrow = 2, ncol = 2,
                                   labels = "AUTO")
  
  ggplot2::ggsave(plot = combo_plot, height = 8, width = 8,
                  filename = file.path(export_folder, paste0(place_short, "_ggplots.png")))
  
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
  agg_lm <- sizer_lm(raw_data = data_sub, sizer_data = sizer_tidy,
                     x = "Year", y =  "FNYield") %>%
    # Then add a column for what bandwidth this is for
    purrr::map(.f = mutate, bandwidth = "aggregate",
               .before = dplyr::everything())
  ## Low bandwidth
  low_lm <- sizer_lm(raw_data = data_sub, sizer_data = sizer_low,
                     x = "Year", y =  "FNYield") %>%
    purrr::map(.f = mutate, bandwidth = band_low,
               .before = dplyr::everything())
  ## Middle bandwidth
  mid_lm <- sizer_lm(raw_data = data_sub, sizer_data = sizer_mid,
                     x = "Year", y =  "FNYield") %>%
    purrr::map(.f = mutate, bandwidth = band_mid,
               .before = dplyr::everything())
  ## High bandwidth
  high_lm <- sizer_lm(raw_data = data_sub, sizer_data = sizer_high,
                     x = "Year", y =  "FNYield") %>%
    purrr::map(.f = mutate, bandwidth = band_high,
               .before = dplyr::everything())
  
  # Form one big list
  mega_lm_list <- list(agg_lm, low_lm, mid_lm, high_lm)
  mega_lm_list[[1]]
  
  # Final dataframe processing for *statistics*
  stat_df <- mega_lm_list %>%
    # Extract first list element
    purrr::map(.f = 1) %>%
    # Make all columns characters
    purrr::map(.f = dplyr::mutate, dplyr::across(dplyr::everything(),
                                          as.character)) %>%
    # Combine all list elements into a dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything())
  
  # Final dataframe processing for *estimates*
 est_df <- mega_lm_list %>%
    purrr::map(.f = 2) %>%
    purrr::map(.f = dplyr::mutate, dplyr::across(dplyr::everything(),
                                                 as.character)) %>%
    purrr::map_dfr(.f = dplyr::select, dplyr::everything())
  
  # Add this information to their respective lists
  giant_list[[paste0("stats_", j)]] <- stat_df
  giant_list[[paste0("estimates_", j)]] <- est_df
  
  # Increase the counter by 1 (for the next iteration of the loop)
  j <- j + 1
  
  # Return a "finished" message!
  message("Processing complete for site: ", place)
}

# Unlist and Export Looped Data ----

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
                             paste0("_", data_type, "_exported.csv")))
  
  # And print a message
  message("Dataframe for ", data_type, " exported.") }

# End ----
