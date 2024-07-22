## ------------------------------------------------------- ##
# Annual Data - Exploratory Graphing
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
rm(list = ls())

# Make a folder for exporting graphs
dir.create(path = file.path("graphs"), showWarnings = F)

# Load custom functions
for(fxn in dir(path = file.path("tools"), pattern = "fxn_")){
  source(file.path("tools", fxn))
}

## And remove loop index object from environment
rm(list = "fxn")

# Load graph helpers
source(file.path("tools", "flow_graph-helpers.R"))

# Identify desired prepared data
prepped_file <- "stats-ready_annual_Conc_uM_DSi.csv"
# prepped_file <- "stats-ready_monthly_Conc_uM_DSi.csv"

# Wrangle that for graph outputs
(graph_prefix <- gsub(pattern = "stats-ready_|\\.csv", replacement = "", x = prepped_file))

# Read in that SiZer output
df_v1 <- read.csv(file = file.path("data", prepped_file)) %>% 
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
  dplyr::arrange(LTER, stream) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, stream, LTER_stream, chemical:section_duration, 
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
  dplyr::arrange(LTER, stream) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(sig_only)

## ----------------------------------------- ##
          # Bookmark Graphs ----
## ----------------------------------------- ##

# Create a bookmark graph for line direction + P value
bookmark_graph(data = core_df, colors = dir_p_palt) +
  theme_high_lat

# Export this graph
ggsave(filename = file.path("graphs", paste0(graph_prefix, "_sig-bookmark.png")),
       height = 8, width = 7, units = "in")




# Basement ----

# DONT USE THIS -- NOT YET REVISITED


## ----------------------------------------- ##
# Plotting Prep ----
## ----------------------------------------- ##

# Grab useful information for informative file names for these graphs
chem <- unique(full_df$chemical)
resp <- gsub(pattern = "_mgL|_uM|_10_6kg_yr|_10_6kmol_yr|_kmol_yr_km2|_kmol_yr|_kg_yr", 
             replacement = "", x = names(full_df)[9])
## Note response identification is dependent upon column order!

# Assemble this into a file prefix
(file_prefix <- paste0("_", chem, "_", resp, "_"))


## ----------------------------------------- ##
# 'Bookmark Graphs' - Full Data ----
## ----------------------------------------- ##

# Count numbers of streams at each LTER
core_hlines <- core_df %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

# Make a graph showing the slope direction and significance for all streams
ggplot(core_df, aes(x = Year, y = stream, color = dir_sig)) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  ## Add 0.5 to number of streams in that LTER and preceding (alphabetical) LTERs
  geom_hline(yintercept = (core_hlines$stream_cumulative + 0.5)) +
  # Customize theme / formatting elements
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph
ggsave(filename = file.path("graphs", paste0("annual_full", file_prefix, "sig-bookmark.png")),
       height = 8, width = 7, units = "in")

# Make the same graph for r2 + slope direction
ggplot(core_df, aes(x = Year, y = stream, color = dir_fit)) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_fit_palt) +
  geom_hline(yintercept = (core_hlines$stream_cumulative + 0.5)) +
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph too
ggsave(filename = file.path("graphs", paste0("annual_full", file_prefix, "fit-bookmark.png")),
       height = 8, width = 7, units = "in")

## ----------------------------------------- ##
# 'Bookmark Graphs' - Sig. Only ----
## ----------------------------------------- ##

# Count numbers of streams at each LTER
sig_hlines <- sig_only %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

# Make a graph showing the slope direction and significance for all streams
ggplot(sig_only, aes(x = Year, y = stream, color = dir_sig)) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  ## Add 0.5 to number of streams in that LTER and preceding (alphabetical) LTERs
  geom_hline(yintercept = (sig_hlines$stream_cumulative + 0.5)) +
  # Customize theme / formatting elements
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph
ggsave(filename = file.path("graphs", paste0("annual_sig-only", file_prefix, "sig-bookmark.png")),
       height = 5, width = 6, units = "in")

# Make the same graph for r2 + slope direction
ggplot(sig_only, aes(x = Year, y = stream, color = dir_fit)) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_fit_palt) +
  geom_hline(yintercept = (sig_hlines$stream_cumulative + 0.5)) +
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph too
ggsave(filename = file.path("graphs", paste0("annual_sig-only", file_prefix, "fit-bookmark.png")),
       height = 5, width = 6, units = "in")

## ----------------------------------------- ##
# Slope + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(stream, section_duration, slope_estimate, slope_std_error) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_simp, aes(x = slope_estimate, y = stream, fill = section_duration)) +
  geom_col() +
  geom_errorbar(aes(xmax = slope_estimate + slope_std_error,
                    xmin = slope_estimate - slope_std_error),
                width = 0.2, linewidth = 0.75, color = "gray66") +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(title = paste("Significant changes in", chem, resp),
       x = "Slope Estimate", y = "Stream") +
  theme_bw()

# Export this graph!
ggsave(filename = file.path("graphs", paste0("annual_sig-only", file_prefix, "slope-duration-barplot.png")),
       width = 6, height = 8, units = "in")

## ----------------------------------------- ##
# Perc. Change + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(stream, section_duration, percent_change) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
# Make an exploratory graph of duration percent change for only significant line chunks
ggplot(sig_simp, aes(x = percent_change, y = stream, fill = section_duration)) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(title = paste("Significant changes in", chem, resp),
       x = "Percent Change (%)", y = "Stream") +
  theme_bw()

# Export this graph!
ggsave(filename = file.path("graphs", paste0("annual_sig-only", file_prefix, "perc-change-duration-barplot.png")),
       width = 6, height = 8, units = "in")


# End ----
