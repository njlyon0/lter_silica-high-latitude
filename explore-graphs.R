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
big_df <- read.csv(file = file.path("sizer_outs", "annual_Yield_kmol_yr_km2_DSi_bw5.csv"))

# Check its structure
dplyr::glimpse(big_df)

## ----------------------------------------- ##
    # Filter SiZer Output Information ----
## ----------------------------------------- ##

# We likely don't want all of the information output by the SiZer workflows
## Need to take special steps to only keep values we care about

# Make one object that is only significant information
sig_only <- big_df %>%
  dplyr::filter(test_p_value < 0.05) %>%
  # Also put in a minimum cutoff for R squareds
  dplyr::filter(r_squared >= 0.30) %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, site) %>%
  # Drop non-unique rows
  dplyr::distinct()

## ----------------------------------------- ##
          # Exploratory Plotting ----
## ----------------------------------------- ##

# Tweak the combination object in preparation for an exploratory plot
combo_v4 <- combo_v3 %>%
  # Abbreviate LTER name if needed
  dplyr::mutate(LTER_abbrev = ifelse(nchar(LTER) > 4,
                                     yes = stringr::str_sub(string = LTER, start = 1, end = 4),
                                     no = LTER), .after = LTER) %>%
  # Make a combination LTER + site information column
  dplyr::mutate(LTER_site = paste0(LTER_abbrev, "_", site), .before = LTER)

# Check structure
sort(unique(combo_v4$LTER_abbrev))
dplyr::glimpse(combo_v4)

# Break off the first bit of the response variable (i.e., drop units)
## Ugly code but it works!
(response_simp <- tidyr::separate_wider_delim(data = data.frame(response_var = response_var), 
                                              cols = response_var, delim = "_", 
                                              too_few = "align_start",
                                              names = c("want", paste0(rep("junk", times = 10), 
                                                                       1:10))) %>%
    dplyr::pull(want))

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
