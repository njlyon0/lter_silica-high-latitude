## ------------------------------------------------------- ##
            # Statistics & Visualization Prep
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# PURPOSE:
## Integrate basin characteristics / climatic "drivers" with the WRTDS data used in SiZer
## Do general wrangling operations required for some statistics / visualization

# Pre-Requisites:
## This script assumes you've run one of the "...-workflow.R" scripts
## And have the relevant output in a "sizer_outs" folder

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, magrittr, supportR)

# Make a folder for combined data / downloading drivers
dir.create(path = file.path("drivers"), showWarnings = F)
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Clear environment
rm(list = ls())


## ----------------------------------------- ##
          # SiZer Output Selection ----
## ----------------------------------------- ##

# This script can handle either annual or seasonal data but you must make that choice here

# Define file name
sizer_filename <- "annual_Discharge_cms_DSi_bw5.csv"
#sizer_filename <- "seasonal_Conc_uM_DSi_bw5.csv"
#sizer_filename <- "monthly_Discharge_cms_DSi_bw5.csv"

# Read in SiZer output data
sizer_v1 <- read.csv(file = file.path("sizer_outs", sizer_filename))

# If this is annual data, add a "season" column so the code can do either
if(!"season" %in% names(sizer_v1)){
  sizer_v1 %<>%
    dplyr::mutate(season = "X", .before = chemical) }
# Note: `%<>%` is a "hinge assignment pipe"
## Equivalent to `df <- df %>% ...`

# If the data doesn't have a "Month" column, add that too
if(!"Month" %in% names(sizer_v1)){
  sizer_v1 %<>%
    dplyr::mutate(Month = "X", .before = chemical) }

# Check structure
dplyr::glimpse(sizer_v1)


## ----------------------------------------- ##
# Latitude Integration ----
## ----------------------------------------- ##

# Read in the site reference table
site_info_v1 <- readr::read_csv('Site_Reference_Table.csv') %>%
  # And subset to only LTERs in the SiZer data
  dplyr::filter(LTER %in% sizer_v1$LTER)

# Check structure
dplyr::glimpse(site_info_v1)

# Pare down the columns the bare minimum of needed information
site_info_v2 <- site_info_v1 %>%
  dplyr::select(LTER, Stream_Name, Latitude) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(site_info_v2)

# Check mismatch of streams with what is in the SiZer data
supportR::diff_check(old = unique(site_info_v2$Stream_Name), new = unique(sizer_v1$Stream_Name))

# Drop unwanted streams
site_info_v3 <- site_info_v2 %>%
  dplyr::filter(Stream_Name %in% unique(sizer_v1$Stream_Name))

# Any sites missing latitude?
site_info_v3 %>%
  dplyr::filter(is.na(Latitude))
## Any sites appearing here need to be edited **in the GoogleSheet "Site_Reference_Table"**
## See the Drive folder here: https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA

# Attach this to the SiZer data
sizer_v2 <- sizer_v1 %>%
  dplyr::left_join(y = site_info_v3, by = c("LTER", "Stream_Name")) %>%
  dplyr::relocate(Latitude, .after = term_p_value)

# Re-check structure
dplyr::glimpse(sizer_v2)

## ----------------------------------------- ##
          # Quality of Life Tweaks ----
## ----------------------------------------- ##

# We'll want a few combination columns to exist for QoL purposes down the line
## Mostly to have easy things to map graphing aesthetics to but there are other benefits!

# Do desired wrangling
sizer_v3 <- sizer_v2 %>%
  # Drop ARC streams
  dplyr::filter(LTER != "ARC") %>%
  # Combine section with stream
  dplyr::mutate(sizer_groups = paste0(stream, "_", section), .before = dplyr::everything()) %>%
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

# Re-check structure
dplyr::glimpse(sizer_v3)

## ----------------------------------------- ##
                  # Export ----
## ----------------------------------------- ##

# Re-name this object
stats_ready2 <- sizer_v3 #%>%
  # And choose a minimum chunk duration for inclusion
  #dplyr::filter(section_duration >= 0)

# Make a file name for this file
(ready_filename_nodrivers <- paste0("stats-ready_nodrivers", sizer_filename))

# Save it locally
write.csv(x = stats_ready2, na = "", row.names = F,
          file = file.path("tidy_data", ready_filename_nodrivers))

# End ----


## ----------------------------------------- ##
# Exploratory plots of Changing values vs Latitude ----
## ----------------------------------------- ##

names(stats_ready)

# Make graph
ggplot(stats_ready, aes(x = Latitude, y = percent_change)) + geom_point() +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_x") +
  geom_smooth(method="lm") +
  # Custom theming / labels
  labs(x = "Latitude", y = "Average Percent Change",
       title = paste("Percent change as function of latitude Si_P conc")) +
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0("Si_P_Conc_vs_Lat", "slope-scatter.png")),
       height = 8, width = 12, units = "in")
