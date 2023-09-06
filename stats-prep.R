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
librarian::shelf(tidyverse, googledrive, supportR)

# Make a folder for combined data / downloading drivers
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Identify / download the driver data
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK")) %>%
  dplyr::filter(name == "all-data_si-extract.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("tidy_data", .$name))

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
          # Driver Data Prep ----
## ----------------------------------------- ##

# Read in the driver data as it is
drivers_v1 <- read.csv(file = file.path("tidy_data", "all-data_si-extract.csv"))

# Check structure
dplyr::glimpse(drivers_v1)

# Split off the static driver information
static_v1 <- drivers_v1 %>%
  dplyr::select(LTER, Stream_Name, 
                elevation_mean_m,
                # dplyr::starts_with("elevation_"), 
                major_rock, 
                # dplyr::starts_with("rocks_"),
                major_land, 
                # dplyr::starts_with("land_"),
                major_soil
                # dplyr::starts_with("soil_")
                )

# Check that out
dplyr::glimpse(static_v1)

# Now split off the dynamic drivers (they'll require more wrangling
dynamic_v1 <- drivers_v1 %>%
  dplyr::select(LTER, Stream_Name, 
                dplyr::starts_with("snow_"), dplyr::starts_with("evapotrans_"),
                dplyr::starts_with("npp_"), dplyr::starts_with("precip")) %>%
  # Drop monthly information of retained dynamic drivers
  dplyr::select(-dplyr::contains("_jan_"), -dplyr::contains("_feb_"), -dplyr::contains("_mar_"),
                -dplyr::contains("_apr_"), -dplyr::contains("_may_"), -dplyr::contains("_jun_"),
                -dplyr::contains("_jul_"), -dplyr::contains("_aug_"), -dplyr::contains("_sep_"),
                -dplyr::contains("_oct_"), -dplyr::contains("_nov_"), -dplyr::contains("_dec_"))

# Check structure
dplyr::glimpse(dynamic_v1)

# Need to summarize this to join appropriately with the SiZer data
dynamic_v2 <- dynamic_v1 %>%
  # Reshape this data into long format for ease of wrangling
  tidyr::pivot_longer(cols = -LTER:-Stream_Name) %>%
  # Clean up the units part of the old column names
  dplyr::mutate(name = gsub(pattern = "_num_days", replacement = "_num.days", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_mm_per_day", replacement = "_mm.per.day", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_kgC_m2_year", replacement = "_kg.C.m2.year", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_kg_m2", replacement = "_kg.m2", x = name)) %>%
  dplyr::mutate(name = gsub(pattern = "_max_prop_area", replacement = "_max.prop.area", x = name)) %>%
  # Break the old name column into its component parts
  tidyr::separate_wider_delim(cols = name, delim = "_", names = c("driver", "Year", "units")) %>%
  # Recombine the driver and units columns
  dplyr::mutate(name_actual = paste(driver, units, sep = "_"), .before = driver) %>%
  dplyr::select(-driver, -units) %>%
  # Make "Year" numeric
  dplyr::mutate(Year = as.numeric(Year)) %>%
  # Average the values within our grouping variables
  dplyr::group_by(dplyr::across(-c(value))) %>%
  dplyr::summarize(value = mean(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Reshape back into wide format with the new name column!
  tidyr::pivot_wider(names_from = name_actual, values_from = value, values_fill = NA)

# Re-check structure
dplyr::glimpse(dynamic_v2)

## ----------------------------------------- ##
          # SiZer Output Prep ----
## ----------------------------------------- ##

# Read in SiZer output data
sizer_v1 <- read.csv(file = file.path("sizer_outs", "annual_Yield_kmol_yr_km2_DSi_bw5.csv"))

# Check structure
dplyr::glimpse(sizer_v1)

## ----------------------------------------- ##
            # Integration Prep ----
## ----------------------------------------- ##

# First, check which LTERs are not in the SiZer data but are in the basin data
supportR::diff_check(old = unique(static_v1$LTER), new = unique(sizer_v1$LTER))
supportR::diff_check(old = unique(dynamic_v2$LTER), new = unique(sizer_v1$LTER))

# Drop any LTERs from the driver data that aren't in our SiZer data
static_v2 <- dplyr::filter(static_v1, LTER %in% unique(sizer_v1$LTER))
dynamic_v3 <- dplyr::filter(dynamic_v2, LTER %in% unique(sizer_v1$LTER))

# Check that fixed the coarsest mismatch
supportR::diff_check(old = unique(static_v2$LTER), new = unique(sizer_v1$LTER))
supportR::diff_check(old = unique(dynamic_v3$LTER), new = unique(sizer_v1$LTER))

# Next, check for any streams that are in drivers but not SiZer and vice versa
## Waited 'til we dropped LTER mismatches to make this result easier to quickly scan
supportR::diff_check(old = unique(static_v2$Stream_Name), new = unique(sizer_v1$Stream_Name))
supportR::diff_check(old = unique(dynamic_v3$Stream_Name), new = unique(sizer_v1$Stream_Name))

# Drop any mismatched streams from the basin data
static_v3 <- dplyr::filter(static_v2, Stream_Name %in% sizer_v1$Stream_Name)
dynamic_v4 <- dplyr::filter(dynamic_v3, Stream_Name %in% sizer_v1$Stream_Name)

# Re-check stream mismatches (should just be McMurdo streams)
supportR::diff_check(old = unique(static_v3$Stream_Name), new = unique(sizer_v1$Stream_Name))
supportR::diff_check(old = unique(dynamic_v4$Stream_Name), new = unique(sizer_v1$Stream_Name))

# Note that these steps aren't totally needed because we're going to do a "left" join
## But still good to be explicit about what streams don't have driver data
## So we're not caught unawares by some missing data they shouldn't be missing

## ----------------------------------------- ##
          # Driver Integration ----
## ----------------------------------------- ##

# Combine the static driver data with the SiZer data!
sizer_v2 <- sizer_v1 %>%
  dplyr::left_join(y = static_v3, by = c("LTER", "Stream_Name"))

# Check structure
dplyr::glimpse(sizer_v2)

# Attach the dynamic drivers too
sizer_v3 <- sizer_v2 %>%
  dplyr::left_join(y = dynamic_v4, by = c("LTER", "Stream_Name", "Year"))

# Re-check structure
dplyr::glimpse(sizer_v3)


# End ----

# Basement (vvv) ----

## ----------------------------------------- ##
# Data Prep ----
## ----------------------------------------- ##

# Grab the desired data file
full_df <- read.csv(file = file.path("sizer_outs", "seasonal_Yield_kmol_yr_km2_DSi_bw5.csv")) %>%
  # Drop ARC streams
  dplyr::filter(LTER != "ARC") %>%
  # Factor season to get the order how we want it
  dplyr::mutate(season = factor(season, 
                                levels = c("winter", "snowmelt", "growing season", "fall"))) %>%
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
    slope_estimate == 0 ~ "none",
    slope_estimate > 0 ~ "pos"),
    .before = slope_estimate) %>%
  # Make combinations of direction + sig. and direction + line fit
  dplyr::mutate(dir_sig = dplyr::case_when(
    slope_direction == "NA" | significance == "NA" ~ "NA",
    significance == "NS" ~ "NS",
    T ~ paste0(slope_direction, "-", significance))) %>%
  dplyr::mutate(dir_fit = dplyr::case_when(
    slope_direction == "NA" | line_fit == "NA" ~ "NA",
    significance == "NS" ~ "NS",
    T ~ paste0(slope_direction, "-", line_fit))) %>%
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great", 
                                            "NA", "NS")))

# Check its structure
dplyr::glimpse(full_df)

# Make a data object with only the columns that we'll want
core_df <- full_df %>%
  # Arrange by LTER, site, and season
  dplyr::arrange(LTER, Stream_Name, season) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, stream, season, chemical:section_duration, 
                F_statistic:line_fit, slope_estimate:slope_std_error,
                dplyr::starts_with("dir_")) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(core_df)

# Filter the simplified data object to only significant rivers with a good fit
sig_only <- core_df %>%
  # Keep only significant slopes
  dplyr::filter(significance %in% c("sig", "marg")) %>%
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(sig_only)

## ----------------------------------------- ##
# Plotting Prep ----
## ----------------------------------------- ##

# Grab useful information for informative file names for these graphs
chem <- unique(full_df$chemical)
resp <- gsub(pattern = "_mgL|_uM|_10_6kg_yr|_10_6kmol_yr|_kmol_yr_km2|_kmol_yr|_kg_yr", 
             replacement = "", x = names(full_df)[10])
## Note response identification is dependent upon column order!

# Assemble this into a file prefix
(file_prefix <- paste0("_", chem, "_", resp, "_"))

# Pick a missing and non significant color
na_col <- "#e5e5e5"
nonsig_col <- "#6c757d"

# Define color palettes
p_palt <- c("NA" = na_col, "sig" = "#132a13",  "marg" = "#006400",  "NS" = nonsig_col)
r2_palt <- c("NA" = na_col,  "bad" = "#b5e48c",  "fine" = "#76c893", 
             "good" = "#1a759f",  "great" = "#184e77")
dir_p_palt <- c("NA" = na_col, "NS" = nonsig_col,
                "pos-sig" = "#ff5400", "pos-marg" = "#ff9e00", 
                "neg-sig" = "#03045e", "neg-marg" = "#00b4d8")
dir_fit_palt <- c("NA" = na_col, "NS" = nonsig_col,
                  "pos-bad" = "#ffe863", "pos-fine" = "#ffe150", 
                  "pos-good" = "#facb2e", "pos-great" = "#f5bd1f",
                  "neg-bad" = "#e4afff", "neg-fine" = "#c86bfa", 
                  "neg-good" = "#722e9a", "neg-great" = "#47297b")

## ----------------------------------------- ##
# 'Bookmark Graphs' - Full Data ----
## ----------------------------------------- ##

# Count numbers of streams at each LTER
core_df %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n())

# Make a graph showing the slope direction and significance for all streams
ggplot(core_df, aes(x = Year, y = stream, color = dir_sig)) +
  # Facet by season
  facet_grid(. ~ season) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  ## Add 0.5 to number of streams in that LTER and preceding (alphabetical) LTERs
  geom_hline(yintercept = 1.5) + # ARC
  geom_hline(yintercept = 22.5) + # Finnish
  geom_hline(yintercept = 28.5) + # GRO
  geom_hline(yintercept = 29.5) + # Kyrcklan
  geom_hline(yintercept = 37.5) + # MCM
  # Customize theme / formatting elements
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph
ggsave(filename = file.path("graphs", paste0("seasonal_full", file_prefix, "sig-bookmark.png")),
       height = 8, width = 15, units = "in")

# Make the same graph for r2 + slope direction
ggplot(core_df, aes(x = Year, y = stream, color = dir_fit)) +
  facet_grid(. ~ season) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_fit_palt) +
  geom_hline(yintercept = 1.5) + # ARC
  geom_hline(yintercept = 22.5) + # Finnish
  geom_hline(yintercept = 28.5) + # GRO
  geom_hline(yintercept = 29.5) + # Kyrcklan
  geom_hline(yintercept = 37.5) + # MCM
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph too
ggsave(filename = file.path("graphs", paste0("seasonal_full", file_prefix, "fit-bookmark.png")),
       height = 8, width = 15, units = "in")

## ----------------------------------------- ##
# 'Bookmark Graphs' - Sig. Only ----
## ----------------------------------------- ##

# Count numbers of streams at each LTER
sig_only %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n())

# Make a graph showing the slope direction and significance for all streams
ggplot(sig_only, aes(x = Year, y = stream, color = dir_sig)) +
  facet_grid(. ~ season) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  ## Add 0.5 to number of streams in that LTER and preceding (alphabetical) LTERs
  geom_hline(yintercept = 8.5) + # Finnish
  geom_hline(yintercept = 12.5) + # GRO
  geom_hline(yintercept = 13.5) + # MCM
  # Customize theme / formatting elements
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph
ggsave(filename = file.path("graphs", paste0("seasonal_sig-only", file_prefix, "sig-bookmark.png")),
       height = 5, width = 12, units = "in")

# Make the same graph for r2 + slope direction
ggplot(sig_only, aes(x = Year, y = stream, color = dir_fit)) +
  facet_grid(. ~ season) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_fit_palt) +
  geom_hline(yintercept = 8.5) + # Finnish
  geom_hline(yintercept = 12.5) + # GRO
  geom_hline(yintercept = 13.5) + # MCM
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph too
ggsave(filename = file.path("graphs", paste0("seasonal_sig-only", file_prefix, "fit-bookmark.png")),
       height = 5, width = 12, units = "in")

## ----------------------------------------- ##
# Slope + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(stream, season, section_duration, slope_estimate, slope_std_error) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_simp, aes(x = slope_estimate, y = stream, fill = section_duration)) +
  facet_grid(. ~ season) +
  geom_col() +
  geom_errorbar(aes(xmax = slope_estimate + slope_std_error,
                    xmin = slope_estimate - slope_std_error),
                width = 0.2, linewidth = 0.75, color = "gray66") +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(title = paste("Significant changes in", chem, resp),
       x = "Slope Estimate", y = "Stream") +
  theme_bw()

# Export this graph!
ggsave(filename = file.path("graphs", paste0("seasonal_sig-only", file_prefix, "slope-duration-barplot.png")),
       height = 8, width = 12, units = "in")

## ----------------------------------------- ##
# Perc. Change + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(stream, season, section_duration, percent_change) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_simp, aes(x = percent_change, y = stream, fill = section_duration)) +
  facet_grid(. ~ season) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(title = paste("Significant changes in", chem, resp),
       x = "Percent Change (%)", y = "Stream") +
  theme_bw()

# Export this graph!
ggsave(filename = file.path("graphs", paste0("seasonal_sig-only", file_prefix, "perc-change-duration-barplot.png")),
       height = 8, width = 12, units = "in")

## ----------------------------------------- ##
# Slope Boxplots ----
## ----------------------------------------- ##

# Pare down the data to only needed information
sig_simp <- sig_only %>%
  dplyr::select(LTER, stream, season, slope_estimate) %>%
  dplyr::distinct()

# Make graph
ggplot(sig_simp, aes(x = season, y = slope_estimate, fill = LTER)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "Season", y = "Slope",
       title = paste("Significant seasonal changes in", chem, resp)) +
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0("seasonal_sig-only", file_prefix, "slope-boxplot.png")),
       height = 8, width = 12, units = "in")

## ----------------------------------------- ##
# Percent Change Boxplots ----
## ----------------------------------------- ##

# Pare down the data to only needed information
sig_simp <- sig_only %>%
  dplyr::select(LTER, stream, season, percent_change) %>%
  dplyr::distinct()

# Make graph
ggplot(sig_simp, aes(x = season, y = percent_change, fill = LTER)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "Season", y = "Percent Change (%)",
       title = paste("Significant seasonal changes in", chem, resp)) +
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0("seasonal_sig-only", file_prefix, "perc-change-boxplot.png")),
       height = 8, width = 12, units = "in")

# End ----
