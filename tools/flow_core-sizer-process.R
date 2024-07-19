## ------------------------------------------------------- ##
# Core SiZer Processing Steps
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

## ----------------------------------------- ##
# Script Explanation ----
## ----------------------------------------- ##

# Purpose:
## The 'actual' SiZer workflow is included below.
## This script is built to be sourced in the '01_sizer-workflow.R' script after some prep steps have been handled

# Rationale for treatment as separate script:
## Essentially we want to do this whole workflow for annual, seasonal, and monthly WRTDS outputs
## No easy way to do that for all three without duplicating this for each context
## SO, rather than doing that duplication, we can source this script at multiple bits
## Also makes updating/maintaining in the future easier

## ----------------------------------------- ##
# Actual Work ----
## ----------------------------------------- ##
# Message for starting of SiZer bit
message("Run SiZer...")

# Invoke the SiZer::SiZer function
sizer_obj <- SiZer::SiZer(x = wrtds_core[[explanatory]], y = wrtds_core[[response]],
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
  place_info <- HERON::id_slope_changes(raw_data = wrtds_core, sizer_data = sizer_info,
                                        x = explanatory, y = response,
                                        group_dig = 5)
  
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
  place_info <- HERON::id_inflections(raw_data = wrtds_core, sizer_data = sizer_info,
                                      x = explanatory, y = response,
                                      group_dig = 5)
  
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
  place_info <- HERON::id_slope_changes(raw_data = wrtds_core, sizer_data = sizer_info,
                                        x = explanatory, y = response,
                                        group_dig = 5)
  
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

# Do some final convenience wrangling
place_export <- place_info %>%
  dplyr::mutate(sizer_bandwidth = 5,
                stream = place, 
                .before = dplyr::everything()) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character))

# Add this tidied dataframe to our export list
data_list[[paste0(place, "_", focal_season, "_", focal_month)]] <- place_export

# Fit linear models
message("Fit regressions...")

# Extract statistics/estimates from linear models
lm_obj <- HERON::sizer_lm(data = place_info, x = explanatory,
                          y = response, group_col = "groups") %>%
  # Then add column for bandwidth
  purrr::map(.f = mutate, sizer_bandwidth = 5,
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
statistic_list[[paste0(place, "_", focal_season, "_", focal_month)]] <- stat_df
estimate_list[[paste0(place, "_", focal_season, "_", focal_month)]] <- est_df

# End ----
