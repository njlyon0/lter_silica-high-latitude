## ------------------------------------------------------- ##
                  # Exploratory Graphing
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# PURPOSE:
## Make exploratory data visualizations
## "Exploratory" in that they may not be publication quality but are still useful tools

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, cowplot)

# Clear environment
rm(list = ls())

# Grab the desired data file
big_df <- read.csv(file = file.path("sizer_outs", "annual_Yield_kmol_yr_km2_DSi_bw5.csv")) %>%
  # Categorize P values
  dplyr::mutate(significance = dplyr::case_when(
    test_p_value < 0.05 ~ "significant",
    test_p_value >= 0.05 & test_p_value <= 0.1 ~ "marginal",
    test_p_value > 0.1 ~ "NS"), .after = test_p_value) %>%
  # Categorize R2 too
  dplyr::mutate(line_fit = dplyr::case_when(
    r_squared < 0.3 ~ "bad",
    r_squared >= 0.3 & r_squared < 0.65 ~ "fine",
    r_squared >= 0.65 & r_squared < 0.8 ~ "good",
    r_squared >= 0.8 ~ "great"), .after = r_squared)

# Check its structure
dplyr::glimpse(big_df)

## ----------------------------------------- ##
        # Data Subset Preparation ----
## ----------------------------------------- ##

# We likely don't want all of the information output by the SiZer workflows
## Need to take special steps to only keep values we care about

# Make one object that is only significant information
sig_only <- big_df %>%
  # Keep only significant slopes
  dplyr::filter(test_p_value <= 0.05) %>%
  # Also put in a minimum cutoff for R squareds
  dplyr::filter(r_squared >= 0.30) %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, site) %>%
  # Pare down to only needed columns
  dplyr::select(LTER, site, stream, chemical, section:section_duration, 
                F_statistic:line_fit, slope_estimate:slope_std_error) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(sig_only)

# Make another that drops columns but doesn't filter out non-sig rows
core_df <- big_df %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, site) %>%
  # Pare down to only needed columns
  dplyr::select(LTER, site, stream, chemical:section_duration, 
                F_statistic:line_fit, slope_estimate:slope_std_error) %>%
  # Drop non-unique rows
  dplyr::distinct() 

# Check structure
dplyr::glimpse(core_df)

## ----------------------------------------- ##
        # Sig Only Visualization ----
## ----------------------------------------- ##

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_only, aes(x = slope_estimate))


# Make the exploratory graph
ggplot(combo_v4, aes(x = estimate, y = LTER_site, fill = duration)) +
  geom_col() +
  geom_errorbar(aes(xmax = estimate + std_error, xmin = estimate - std_error), 
                width = 0.2, linewidth = 0.75, color = "gray66") +
  labs(title = paste("Significant Changes in", element, response_simp),
       x = "Estimate", y = "LTER Abbreviation") +
  theme_bw()

# Export this graph
ggsave(filename = file.path(export_folder, 
                            paste0("_SEASONAL_sig-sizer-barplot_", Sys.Date(), ".png")),
       width = 6, height = 8, units = "in")


# End ----
