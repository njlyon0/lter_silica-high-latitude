## ------------------------------------------------------- ##
# Greenup Day Analysis & Graphing
## ------------------------------------------------------- ##
# Purpose:
## Process green up day data and perform requested analysis

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, emmeans, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("data", "stats-results"), showWarnings = F, recursive = T)
dir.create(path = file.path("graphs", "greenup"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Read in the greenup data
gup_v01 <- read.csv(file = file.path("data", "map-data", "si-extract_greenup_2_v061.csv"))

# Check structure
dplyr::glimpse(gup_v01)

# Also grab one discharge data file
q_v01 <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Discharge_cms_DSi.csv"))
dplyr::glimpse(q_v01)

## ----------------------------------------- ##
# Data Processing ----
## ----------------------------------------- ##

# Wrangle green-up data as needed
gup_v02 <- gup_v01 %>% 
  # Pare down to only wanted
  dplyr::select(LTER, Stream_Name, dplyr::contains("cycle0")) %>% 
  # Filter to only streams in discharge data
  dplyr::filter(Stream_Name %in% q_v01$Stream_Name) %>% 
  # Make all empty cells into real NAs
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .))) %>% 
  # Flip to long format & ditch NAs
  tidyr::pivot_longer(cols = dplyr::starts_with("greenup"),
    names_to = "cycle", values_to = "greenup_date") %>% 
  dplyr::filter(!is.na(greenup_date)) %>% 
  # Calculate desired date info
  dplyr::mutate(greenup_date = as.Date(greenup_date)) %>% 
  dplyr::mutate(year = lubridate::year(x = greenup_date),
    .after = cycle) %>% 
  dplyr::mutate(greenup_doy = lubridate::yday(greenup_date))

# Check structure
dplyr::glimpse(gup_v02)

# Sanity check: do we have the same number of discharge streams as greenup day streams?
## Excluding McMurdo because we know we don't have spatial data for those rivers
length(unique(gup_v02$Stream_Name))
q_v01 %>% dplyr::filter(LTER != "MCM") %>% pull(Stream_Name) %>% unique() %>% length()

## ----------------------------------------- ##
# Statistics ----
## ----------------------------------------- ##

# Fit desired model
gup_lm <- lm(greenup_doy ~ year * LTER, data = gup_v02)

# Strip desired summary info into tidy table
gup_results <- as.data.frame(stats::anova(object = gup_lm)) %>% 
  tibble::rownames_to_column(.data = ., var = "term") %>% 
  dplyr::select(-`Sum Sq`, -`Mean Sq`) %>% 
  dplyr::rename(deg_free = Df,
    f_stat = `F value`,
    p_value = `Pr(>F)`) %>% 
  dplyr::filter(term != "Residuals") %>% 
  dplyr::mutate(sig = ifelse(test = p_value < 0.05, 
    yes = "yes", no = "no"))

# Check what that yields
gup_results

# Export it locally
write.csv(x = gup_results, na = '', row.names = F,
  file = file.path("data", "stats-results", "greenup-lm_results.csv"))

# Get the coefficients
gup_coef <- data.frame(group = names(gup_lm$coefficients),
    coef = gup_lm$coefficients) %>% 
  dplyr::mutate(test = "greenup", .before = dplyr::everything())

# Check that out
dplyr::glimpse(gup_coef)

# Export it locally
write.csv(x = gup_coef, na = '', row.names = F,
  file = file.path("data", "stats-results", "greenup-lm_coefficients.csv"))

# Get pairwise comparisons
gup_pair <- as.data.frame(emmeans::emtrends(object = gup_lm, pairwise ~ LTER, 
      var = "year")$contrasts) %>% 
    dplyr::mutate(sig = ifelse(test = p.value < 0.05, 
      yes = "yes", no = "no"))    

# Check what that yields
gup_pair

# Export it locally
write.csv(x = gup_pair, na = '', row.names = F,
  file = file.path("data", "stats-results", "greenup-lm_pair-results.csv"))

## ----------------------------------------- ##
# Graph(s) ----
## ----------------------------------------- ##
# Load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Summarize gup data
gup_smry <- supportR::summary_table(data = gup_v02, groups = c("LTER", "year"),
  response = "greenup_doy", drop_na = T)

# Check that out quickly
gup_smry

# Make graph!
stick_graph(data = gup_smry, resp_var = "mean",  
    exp_var = "year", sig = "ixn",
    lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "Green-Up Day of Year)",
    x = "Year") +
  theme(legend.position = "inside",
    legend.position.inside = c(0.85, 0.85),
    axis.text = element_text(color = "black"))

# Export this locally
ggplot2::ggsave(filename = file.path("graphs", "greenup", "sticks_greenup-day-of-year.png"),
  height = 5, width = 5, units = "in")

# End ----
