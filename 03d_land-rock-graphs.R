## ------------------------------------------------------- ##
# Lithology & Land Cover Graphs
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# Purpose:
## Simple graphs of element concentration against lithology/land cover

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "land-rock"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

## ----------------------------------------- ##
# Data Prep ----
## ----------------------------------------- ##

# Read in necessary data file(s)
df_conc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                               pattern = "_Conc_uM_"),
                      .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  # Stack them vertically
  purrr::list_rbind(x = .) %>% 
  # Do needed subsetting
  dplyr::filter(!LTER %in% c("Canada")) %>%
  dplyr::filter(chemical %in% c("Si_P", "Si_DIN") != T) %>% 
  # tidy "P" to match other graphs
  dplyr::mutate(chemical = ifelse(chemical == "P",
                                  yes = "DIP", no = chemical)) %>% 
  # Pare down to needed columns
  dplyr::select(LTER:LTER_stream, 
                major_rock, major_land,
                chemical, Year, Conc_uM) %>% 
  dplyr::distinct() %>% 
  # Fill McMurdo categories
  dplyr::mutate(major_rock = ifelse(LTER == "MCM", yes = "ice", no = major_rock),
                major_land = ifelse(LTER == "MCM", yes = "ice", no = major_land))

# Check structure
dplyr::glimpse(df_conc)

# Check counts of rock/land
supportR::count(df_conc$major_rock)
supportR::count(df_conc$major_land)

## ----------------------------------------- ##
# Rock Graph ----
## ----------------------------------------- ##

# Make desired graph
ggplot(df_conc, aes(x = reorder(major_rock, -Conc_uM), y = Conc_uM, fill = major_rock)) +
  geom_jitter(width = 0.25, alpha = 0.2, pch = 21) +
  geom_violin(alpha = 0.6) +
  facet_grid(chemical ~ ., axes = "all_y", scales = "free_y") +
  labs(x = "Dominant Lithology", y = "Concentration (uM)") +
  scale_fill_manual(values = c("metamorphic" = "#8338ec",
                               "plutonic" = "#fb5607",
                               "plutonic; metamorphic" = "#ff006e",
                               "sedimentary" = "#dda15e",
                               "ice" = "#8ecae6",
                               "volcanic" = "#c1121f")) +
  theme_facetbox +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(file.path("graphs", "land-rock", "conc-by-rock.png"),
                 height = 7, width = 6, units = "in")

## ----------------------------------------- ##
# Land Graph ----
## ----------------------------------------- ##

# Make desired graph
ggplot(df_conc, aes(x = reorder(major_land, -Conc_uM), y = Conc_uM, fill = major_land)) +
  geom_jitter(width = 0.25, alpha = 0.2, pch = 21) +
  geom_violin(alpha = 0.6) +
  facet_grid(chemical ~ ., axes = "all_y", scales = "free_y") +
  labs(x = "Dominant Lithology", y = "Concentration (uM)") +
  scale_fill_manual(values = c("evergreen_needleleaf_forest" = "#31572c",
                               "mixed_forest" = "#4f772d",
                               "deciduous_needleleaf_forest" = "#90a955",
                               "tundra" = "#b0c4b1",
                               "cropland" = "#ffb703",
                               "ice" = "#8ecae6",
                               "shrubland_grassland" = "#ff4d6d")) +
  theme_facetbox +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Export locally
ggsave(file.path("graphs", "land-rock", "conc-by-land.png"),
       height = 7, width = 6, units = "in")

# End ----
