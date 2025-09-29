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
dir.create(path = file.path("graphs", "figures"), showWarnings = F)

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
  dplyr::mutate(major_rock = ifelse(LTER == "MCM", yes = "glacial drift", no = major_rock),
                major_land = ifelse(LTER == "MCM", yes = "barren", no = major_land)) %>% 
  # Tidy up categories slightly
  dplyr::mutate(major_land = gsub("_", " ", x = major_land)) %>% 
  dplyr::mutate(major_land = gsub("shrubland grassland", "shrubland / grassland", x = major_land)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("major_"),
                              .fns = stringr::str_to_title))

# Check structure
dplyr::glimpse(df_conc)

# Check counts of rock/land
supportR::count(df_conc$major_rock)
supportR::count(df_conc$major_land)

## ----------------------------------------- ##
# Rock Graph ----
## ----------------------------------------- ##

# Make desired graph
graph_rock <- ggplot(df_conc, aes(x = reorder(major_rock, -Conc_uM), 
                                  y = Conc_uM, fill = major_rock)) +
  geom_jitter(width = 0.25, alpha = 0.2, pch = 21) +
  geom_violin(alpha = 0.6) +
  facet_grid(chemical ~ ., axes = "all_y", scales = "free_y") +
  labs(x = "Dominant Lithology", y = "Concentration (uM)") +
  scale_fill_manual(values = c("Metamorphic" = "#8338ec",
                               "Plutonic" = "#fb5607",
                               "Plutonic; Metamorphic" = "#ff006e",
                               "Sedimentary" = "#dda15e",
                               "Glacial Drift" = "#8ecae6",
                               "Volcanic" = "#c1121f")) +
  theme_facetbox +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Check it out
graph_rock

# Export locally
ggsave(file.path("graphs", "land-rock", "conc-by-rock.png"),
                 height = 7, width = 8, units = "in")

## ----------------------------------------- ##
# Land Graph ----
## ----------------------------------------- ##

# Make desired graph
graph_land <- ggplot(df_conc, aes(x = reorder(major_land, -Conc_uM),
                                  y = Conc_uM, fill = major_land)) +
  geom_jitter(width = 0.25, alpha = 0.2, pch = 21) +
  geom_violin(alpha = 0.6) +
  facet_grid(chemical ~ ., axes = "all_y", scales = "free_y") +
  labs(x = "Dominant Lithology", y = "Concentration (uM)") +
  scale_fill_manual(values = c("Evergreen Needleleaf Forest" = "#31572c",
                               "Mixed Forest" = "#4f772d",
                               "Deciduous Needleleaf Forest" = "#90a955",
                               "Tundra" = "#b0c4b1",
                               "Cropland" = "#ffb703",
                               "Barren" = "#8ecae6",
                               "Shrubland / Grassland" = "#ff4d6d")) +
  theme_facetbox +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# Check it out
graph_land

# Export locally
ggsave(file.path("graphs", "land-rock", "conc-by-land.png"),
       height = 7, width = 8, units = "in")

## ----------------------------------------- ##
# Assemble Multi-Panel Figure ----
## ----------------------------------------- ##

# Minor tweaks to each graph
## Drop redundant strip labels for left side graph
graph_land_v2 <- graph_land +
  theme(strip.text = element_blank())
## Drop redundant Y-axis labels for rightside graph
graph_rock_v2 <- graph_rock +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Combine these graphs into one figure
cowplot::plot_grid(graph_land_v2, graph_rock_v2, nrow = 1, align = "h")

# Export locally
ggsave(file.path("graphs", "figures", "fig_conc-by-land-and-rock.png"),
       height = 7, width = 14, units = "in")

# End ----
