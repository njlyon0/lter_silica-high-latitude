## ------------------------------------------------------- ##
                  # Exploratory Graphing
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Make exploratory graphs for statistics-ready data
## "Exploratory" in that they may not be publication quality but are still useful tools

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cowplot)

# Clear environment
rm(list = ls()); gc()

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "explore"), showWarnings = F, recursive = T)

# Load custom functions
purrr::walk(.x = dir(path = file.path("tools"), pattern = "fxn_"),
            .f = ~ source(file.path("tools", .x)))

# Load graph helpers
source(file.path("tools", "flow_graph-helpers.R"))

# Identify desired prepared data
# prepped_file <- "stats-ready_annual_Conc_uM_DSi.csv"
prepped_file <- "stats-ready_monthly_Conc_uM_DSi.csv"

# Wrangle that for graph outputs
(graph_prefix <- gsub(pattern = "stats-ready_|\\.csv", replacement = "", x = prepped_file))

# Read in that SiZer output
df_v1 <- read.csv(file = file.path("data", "stats-ready_monthly", prepped_file)) %>% 
  # Replace NAs with "NA" in some columns
  dplyr::mutate(dplyr::across(.cols = c(dir_sig, dir_fit),
                              .fns = ~ ifelse(test = is.na(.x) == T,
                                              yes = "NA", no = .x)) ) %>% 
  # Make certain columns into factors with ordered levels
  dplyr::mutate(dir_sig = factor(dir_sig, levels = dir_sig_levels),
                dir_fit = factor(dir_fit, levels = dir_fit_levels))

# Check structure
dplyr::glimpse(df_v1)

## ----------------------------------------- ##
                  # Data Prep ----
## ----------------------------------------- ##
# Disclaimer:
## Yes, most of this is handled in '02_stats-prep.R'
## However, some tweaks make more sense to do directly before exploratory visualization

# Make a data object with only the columns that we'll want
core_df <- df_v1 %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical:section_duration, 
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
      # Bookmark Graphs - Full Data ----
## ----------------------------------------- ##

# Create a bookmark graph for line direction + P value
bookmark_graph(data = core_df, color_var = "dir_sig", colors = dir_p_palt) +
  theme_high_lat

# Export this graph
ggsave(filename = file.path("graphs", "explore", paste0(graph_prefix, "_full-data_sig-bookmark.png")),
       height = 8, width = 7, units = "in")

# Make another for line direction & R2 (instead of P value)
bookmark_graph(data = core_df, color_var = "dir_fit", colors = dir_fit_palt) +
  theme_high_lat

# Export this graph
ggsave(filename = file.path("graphs", "explore", paste0(graph_prefix, "_full-data_fit-bookmark.png")),
       height = 8, width = 7, units = "in")

## ----------------------------------------- ##
# Bookmark Graphs - Significant Only ----
## ----------------------------------------- ##

# Create a bookmark graph for line direction + P value
bookmark_graph(data = sig_only, color_var = "dir_sig", colors = dir_p_palt) +
  theme_high_lat

# Export this graph
ggsave(filename = file.path("graphs", "explore", paste0(graph_prefix, "_sig-data_sig-bookmark.png")),
       height = 8, width = 7, units = "in")

# Make another for line direction & R2 (instead of P value)
bookmark_graph(data = sig_only, color_var = "dir_fit", colors = dir_fit_palt) +
  theme_high_lat

# Export this graph
ggsave(filename = file.path("graphs", "explore", paste0(graph_prefix, "_sig-data_fit-bookmark.png")),
       height = 8, width = 7, units = "in")

## ----------------------------------------- ##
    # Slope + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(Stream_Name, season, Month, section_duration, 
                slope_estimate, slope_std_error) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_simp, aes(x = slope_estimate, y = Stream_Name, fill = section_duration)) +
  geom_col() +
  geom_errorbar(aes(xmax = slope_estimate + slope_std_error,
                    xmin = slope_estimate - slope_std_error),
                width = 0.2, linewidth = 0.75, color = "gray66") +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(x = "Slope Estimate", y = "Stream", fill = "Duration (years)") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.85))

# Export this graph!
ggsave(filename = file.path("graphs", "explore", paste0(graph_prefix, "_slope-duration-barplot.png")),
       width = 12, height = 8, units = "in")

## ----------------------------------------- ##
# Perc. Change + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(Stream_Name, season, Month, section_duration, 
                percent_change) %>%
  dplyr::distinct()

# Make an exploratory graph of duration percent change for only significant line chunks
ggplot(sig_simp, aes(x = percent_change, y = Stream_Name, fill = section_duration)) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(x = "Slope Estimate", y = "Stream", fill = "Duration (years)") +
  theme_bw() +
  theme(legend.position = "inside", legend.position.inside = c(0.1, 0.15))

# Export this graph
ggsave(filename = file.path("graphs", "explore", paste0(graph_prefix, "_perc-change-duration-barplot.png")),
       width = 12, height = 8, units = "in")

# End ----
