## ------------------------------------------------------- ##
          # `SiZer` Workflow for Silica Export WG
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(broom, cowplot, googledrive, SiZer, tidyverse, lter/HERON)

# Clear environment
rm(list = ls())

# Identify files in Drive folder
ids <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/folders/1842KSgp48k_DwvNeYbmz-_b4PSH-vrxg"))

# Create a folder to save to
dir.create(path = "data", showWarnings = F)

# Download desired data
ids %>%
  dplyr::filter(name == "Full_Results_ResultsTable_GFN_WRTDS.csv") %>%
  googledrive::drive_download(file = googledrive::as_id(.),
                              path = file.path("data", "Full_Results_ResultsTable_GFN_WRTDS.csv"),
                              overwrite = T)

# Load data
data_v0 <- readr::read_csv(file = file.path("data", "Full_Results_ResultsTable_GFN_WRTDS.csv"))

# Now subset to sites of interest
data <- data_v0 %>%
  # Keep only polar sites
  dplyr::filter(LTER %in% c("MCM", "ARC", "GRO", "Finnish Environmental Institute","NIVA") | stream %in% c("Site 7")) %>%
  # But drop one site that is technically polar
  dplyr::filter(stream != "Site 69038")

# Take a look!
dplyr::glimpse(data)

# Clean up environment
rm(list = setdiff(ls(), c("data")))

## ----------------------------------------- ##
          # Pre-Loop Preparation ----
## ----------------------------------------- ##

# Identify response (Y) and explanatory (X) variables
response_var <- "Flux_10_6kmol_yr"
## Yield, Conc_uM, Discharge_cms
explanatory_var <- "Year"

# Identify which chemical you want to analyze
element <- "Si:P"
## DSi, Si:DIN, Si:P

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

# Identify the bandwidth to use with SiZer
bandwidth <- 5

# Create a folder to save experimental outputs
# Folder name is: [response]_bw[bandwidths]_[date]
(export_folder <- paste0("export_", response_var, "_",
                         element, "_bw", bandwidth,
                         "_", Sys.Date()))
dir.create(path = export_folder, showWarnings = FALSE)

# Make an empty list to store all of our extracted information
giant_list <- list()

# Make a counter and set it to 1 (the list will add to it)
j <- 1

## ----------------------------------------- ##
      # Loop Extract of SiZer Data ----
## ----------------------------------------- ##

# Subset the data to only streams that have the chemical of interest
data_short <- data %>%
  dplyr::filter(chemical == element)

# Loop through sites and extract information
for(place in unique(data_short$stream)) {
  # for(place in "Site 7"){
  
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
  png(filename = file.path(export_folder, paste0(place_short, "_SiZer-plot.png")), width = 5, height = 5, res = 720, units = 'in')
  HERON::sizer_plot(sizer_object = e,
                    bandwidth_vec = c(bandwidth))
  dev.off()
  
  # Identify inflection points/slope changes
  sizer_info <- HERON::sizer_slice(sizer_object = e, bandwidth = bandwidth)
  
  # Loop - Identify Slope Changes ----
  message("Find slope changes...")
  
  # Identify changes in the slope
  data_sub <- HERON::id_slope_changes(raw_data = data_sub,
                                      sizer_data = sizer_info,
                                      x = explanatory_var,
                                      y = response_var)
  
  # Loop - Plotting ----
  message("Making plots...")
  
  # Handle three possible types of changes
  ## 1. No inflection point
  if(nrow(sizer_info) == 0){
    demo_plot <- HERON::sizer_ggplot(raw_data = data_sub,
                                     sizer_data = sizer_info,
                                     x = explanatory_var, y = response_var,
                                     trendline = 'sharp', vline = "none",
                                     sharp_colors = c("#bbbbbb", 'purple')) +
      ggtitle(label = paste0("h = ", bandwidth, " Slope Changes (None)"))
  } else {
  ## 2. Slope changes (+/0, -/0, 0/+, 0/-) without full inflection point (+/-, -/+)
  if(is.na(sizer_info$pos_to_neg) & is.na(sizer_info$neg_to_pos)){
    demo_plot <- HERON::sizer_ggplot(raw_data = data_sub,
                                     sizer_data = sizer_info,
                                     x = explanatory_var, y = response_var,
                                     trendline = 'sharp', vline = "changes",
                                     sharp_colors = c("#bbbbbb", 'purple')) +
      ggtitle(label = paste0("h = ", bandwidth, " Slope Changes"))
  }
  ## 3. Full inflection point(s) (+/-, -/+) found
  if(!is.na(sizer_info$pos_to_neg) | !is.na(sizer_info$neg_to_pos)){
    demo_plot <- HERON::sizer_ggplot(raw_data = data_sub,
                                     sizer_data = sizer_info,
                                     x = explanatory_var, y = response_var,
                                     trendline = 'sharp', vline = "inflections",
                                     sharp_colors = c("#bbbbbb", 'purple')) +
      ggtitle(label = paste0("h = ", bandwidth, " Inflection Points"))
  } }
  
  # Export whichever graph got made
  ggplot2::ggsave(filename = file.path(export_folder, 
                                       paste0("slope-change_", place_short, "_ggplots.png")),
                  height = 8, width = 8)
  
  # Loop - Wrangle SiZer Data ----
  message("Wrangling SiZer data...")
  
  # Modify the columns in the provided sizer dataframes
  sizer_export <- data_sub %>%
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
  lm_obj <- HERON::sizer_lm(data = data_sub, x = explanatory_var,
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
      # Unlist and Export Looped Data ----
## ----------------------------------------- ##

# Check out what is in our huge list
names(giant_list)

# Now (ironically) we'll use a loop to unlist what the first loop made
for(data_type in c("data", "stats", "estimates")){
  
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
