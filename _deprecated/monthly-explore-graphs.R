## ------------------------------------------------------- ##
          # Monthly Data - Exploratory Graphing
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# PURPOSE:
## Make exploratory data visualizations for *monthly* data
## "Exploratory" in that they may not be publication quality but are still useful tools

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, cowplot)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
              # Data Prep ----
## ----------------------------------------- ##

# Identify the filename
file_name <- "stats-ready_monthly_Conc_uM_DSi_bw5.csv"

# Grab the desired data file
full_v0 <- read.csv(file = file.path("tidy_data", file_name))

# Glimpse it
dplyr::glimpse(full_v0)

# Do some processing
full_df <- full_v0 %>% 
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
  # Arrange by LTER, site, and month
  dplyr::arrange(LTER, Stream_Name, Month) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, stream, Month, 
                chemical:section_duration, 
                F_statistic:line_fit, 
                slope_estimate:slope_std_error,
                dplyr::starts_with("dir_")) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(core_df)

# What do we drop with that operation?
supportR::diff_check(old = names(full_df), new = names(core_df))

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
core_hlines <- core_df %>%
  dplyr::select(LTER, stream) %>%
  dplyr::distinct() %>%
  dplyr::group_by(LTER) %>%
  dplyr::summarize(stream_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stream_cumulative = cumsum(x = stream_ct))

# Make a graph showing the slope direction and significance for all streams
ggplot(core_df, aes(x = Year, y = stream, color = dir_sig)) +
  # Facet by month
  facet_grid(. ~ Month) +
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
ggsave(filename = file.path("graphs", paste0("monthly_full", file_prefix, "sig-bookmark.png")),
       height = 8, width = 20, units = "in")

# Make the same graph for r2 + slope direction
ggplot(core_df, aes(x = Year, y = stream, color = dir_fit)) +
  facet_grid(. ~ Month) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_fit_palt) +
  geom_hline(yintercept = (core_hlines$stream_cumulative + 0.5)) +
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph too
ggsave(filename = file.path("graphs", paste0("monthly_full", file_prefix, "fit-bookmark.png")),
       height = 8, width = 20, units = "in")

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
  facet_grid(. ~ Month) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_p_palt) +
  # Put in horizontal lines between LTERs
  geom_hline(yintercept = (sig_hlines$stream_cumulative + 0.5)) +
  # Customize theme / formatting elements
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "sig-bookmark.png")),
       height = 6, width = 20, units = "in")

# Make the same graph for r2 + slope direction
ggplot(sig_only, aes(x = Year, y = stream, color = dir_fit)) +
  facet_grid(. ~ Month) +
  geom_path(aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
  scale_color_manual(values = dir_fit_palt) +
  geom_hline(yintercept = (sig_hlines$stream_cumulative + 0.5)) +
  labs(x = "Year", y = "Stream") +
  theme_bw() +
  theme(legend.title = element_blank())

# Export this graph too
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "fit-bookmark.png")),
       height = 6, width = 20, units = "in")

## ----------------------------------------- ##
      # Slope + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(stream, Month, section_duration, slope_estimate, slope_std_error) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_simp, aes(x = slope_estimate, y = stream, fill = section_duration)) +
  facet_grid(. ~ Month) +
  geom_col() +
  geom_errorbar(aes(xmax = slope_estimate + slope_std_error,
                    xmin = slope_estimate - slope_std_error),
                width = 0.2, linewidth = 0.75, color = "gray66") +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(title = paste("Significant changes in", chem, resp),
       x = "Slope Estimate", y = "Stream") +
  theme_bw()

# Export this graph!
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "slope-duration-barplot.png")),
       height = 8, width = 12, units = "in")

## ----------------------------------------- ##
  # Perc. Change + Duration - Sig. Only ----
## ----------------------------------------- ##

# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(stream, Month, section_duration, percent_change) %>%
  dplyr::distinct()

# Make an exploratory graph of duration for only significant line chunks
ggplot(sig_simp, aes(x = percent_change, y = stream, fill = section_duration)) +
  facet_grid(. ~ Month) +
  geom_col() +
  geom_vline(xintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  labs(title = paste("Significant changes in", chem, resp),
       x = "Percent Change (%)", y = "Stream") +
  theme_bw()

# Export this graph!
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "perc-change-duration-barplot.png")),
       height = 8, width = 20, units = "in")

## ----------------------------------------- ##
            # Slope Boxplots ----
## ----------------------------------------- ##

# Pare down the data to only needed information
sig_simp <- sig_only %>%
  dplyr::select(LTER, stream, Month, slope_estimate) %>%
  dplyr::distinct()

# Make graph
ggplot(sig_simp, aes(x = as.factor(Month), y = slope_estimate, fill = LTER)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "Month", y = "Slope",
       title = paste("Significant monthly changes in", chem, resp)) +
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "slope-boxplot.png")),
       height = 8, width = 12, units = "in")

## ----------------------------------------- ##
        # Percent Change Boxplots ----
## ----------------------------------------- ##

# Pare down the data to only needed information
sig_simp <- sig_only %>%
  dplyr::select(LTER, stream, Month, percent_change) %>%
  dplyr::distinct()

# Make graph
ggplot(sig_simp, aes(x = as.factor(Month), y = percent_change, fill = LTER)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "Month", y = "Percent Change (%)",
       title = paste("Significant monthly changes in", chem, resp)) +
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "perc-change-boxplot.png")),
       height = 8, width = 20, units = "in")

# End ----
