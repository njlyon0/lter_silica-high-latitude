## ------------------------------------------------------- ##
                  # High Latitude Site Map
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Create publication-quality site map figure

# Pre-Requisites:
## This script assumes you've run *both* "00_data-download.R" and "02_stats-prep.R"

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, maps, terra, stars, supportR, cowplot)

# Get around weird issue with scientific notation
options(scipen = 10000)

# Clear environment & do garbage collection too
rm(list = ls()); gc()

# Make a folder for exporting graphs
dir.create(path = file.path("graphs"), showWarnings = F)

# Identify desired prepared data
prepped_file <- "stats-ready_annual_Conc_uM_DSi.csv"

# Read in that SiZer output
df_v1 <- read.csv(file = file.path("data", "stats-ready_annual", prepped_file)) %>%
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream "))

# Check structure
dplyr::glimpse(df_v1)

## ------------------------------------------ ##
            # Coordinate Prep ----
## ------------------------------------------ ##

# Process the pre-wrangled data to be ready for use in map-making
site_df <- df_v1 %>%
  # Rename lat/long columns
  dplyr::rename(lat = Latitude, lon = Longitude) %>% 
  # Summarize within groups
  dplyr::group_by(LTER_stream, LTER, Stream_Name, drainSqKm, lat, lon) %>% 
  dplyr::summarize(mean_si = mean(Conc_uM, na.rm = T),
                   mean_abs_change = mean(abs(percent_change), na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(site_df)
  
## ------------------------------------------ ##
# Permafrost Prep ----
## ------------------------------------------ ##

# For more information on permafrost data see:
## https://apgc.awi.de/dataset/pex
## Note that I re-projected into WGS84 to match CRS of rivers

# Need to process permafrost raster if it's not already present
if("permafrost-simple.tif" %in% dir(path = file.path("data", "map-data")) != T){
  
  # Read in permafrost data
  pf <- terra::rast(x = file.path("data", "map-data", "permafrost-probability.tif"))
  
  # Exploratory plot to make sure it looks OK
  plot(pf, axes = T, main = "Permafrost probability")
  
  # Identify permafrost probability threshold we want to use as a cutoff
  pf_thresh <- 0.6
  
  # Subset to only permafrost probabilities above a certain threshold
  pf_v2 <- terra::clamp(x = pf, lower = pf_thresh, upper = 1, values = F)
  
  # Do another plot to see that worked
  plot(pf_v2, axes = T, 
       main = paste0("Permafrost probability ≥", (pf_thresh * 100), "%"))
  
  # And coerce all values above the threshold to a single value
  pf_v3 <- terra::clamp(x = pf_v2, lower = 1, upper = 1, values = T)
  
  # One more demo plot
  plot(pf_v3, axes = T, 
       main = paste0("Permafrost probability ≥", (pf_thresh * 100), "% (set to 1)"))
  
  # Export our modified raster
  terra::writeRaster(x = pf_v3, overwrite = T,
                     filename = file.path("data", "map-data", "permafrost-simple.tif"))
  
  # Clean up environment & collect garbage
  rm(list = setdiff(x = ls(), y = c("site_df"))); gc() }

## ------------------------------------------ ##
              # Site Map Prep ----
## ------------------------------------------ ##

# Read in permafrost data
pf_stars <- stars::read_stars(.x = file.path("data", "map-data", "permafrost-simple.tif"))

# Get country & US state borders
borders <- sf::st_as_sf(maps::map(database = "world", plot = F, fill = T)) %>%
  dplyr::bind_rows(sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)))

# Split our site information into polar regions
low_lats <- dplyr::filter(site_df, lat < 0)
high_lats <- dplyr::filter(site_df, lat > 0)

# Check range of lat/long for all limits
range(low_lats$lat); range(low_lats$lon)
range(high_lats$lat); range(high_lats$lon)

# Define borders of maps so that the site points will be nicely inside the borders
low_lims <- list("lat" = c(-80, -60), "lon" = c(-180, 180))
high_lims <- list("lat" = c(55, 80), "lon" = c(-170, 170)) 

# Define the bits of the map that all sub-maps will want to re-use
core_map <-  borders %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  # Add permafrost to this section
  stars::geom_stars(data = pf_stars, downsample = 10) +
  # Customize some global (ha ha) theme/formatting bits
  # labs(x = "Longitude", y = "Latitude") +
  labs(x = "", y = "") +
  # scale_fill_continuous(na.value = "transparent") +
  supportR::theme_lyon(text_size = 13)

# Make the same graph for high latitude sites
map_high <- core_map +
  # Set map extent
  coord_sf(xlim = high_lims[["lon"]], ylim = high_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aesthetics
  geom_point(data = site_df, aes(x = lon, y = lat, color = mean_si,
                                 size = drainSqKm), shape = 19, alpha = 0.75) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = high_lims[["lon"]], 
                     breaks = seq(from = min(high_lims[["lon"]]), 
                                  to = max(high_lims[["lon"]]), 
                                  by = 50)) + 
  scale_y_continuous(limits = high_lims[["lat"]], 
                     breaks = seq(from = min(high_lims[["lat"]]) + 5, 
                                  to = max(high_lims[["lat"]]), 
                                  by = 15)) + 
  # Customize fill & color
  scale_fill_gradient(low = "#8ecae6", high = "#8ecae6", na.value = "transparent") +
  scale_color_gradient(low = "#780116", high = "#f7b538") +
  # Customize point size bins
  scale_size_binned(breaks = c(10^3, 10^6)) +
  theme(legend.position = "none") +
  guides(fill = "none"); map_high

# Make the low latitude map
map_low <- core_map +
  coord_sf(xlim = low_lims[["lon"]], ylim = low_lims[["lat"]], expand = F) +
  geom_point(data = site_df, aes(x = lon, y = lat, color = mean_si, 
                                 size = drainSqKm), shape = 19, alpha = 0.75) +
  scale_x_continuous(limits = low_lims[["lon"]], 
                     breaks = seq(from = min(low_lims[["lon"]]), 
                                  to = max(low_lims[["lon"]]), 
                                  by = 50)) + 
  scale_y_continuous(limits = low_lims[["lat"]], 
                     breaks = seq(from = min(low_lims[["lat"]]), 
                                  to = max(low_lims[["lat"]]), 
                                  by = 15)) + 
  scale_fill_gradient(low = "#8ecae6", high = "#8ecae6", na.value = "transparent") +
  scale_color_gradient(low = "#780116", high = "#f7b538") +
  scale_size_binned(breaks = c(10^3, 10^6)) +
  # Put legend above map
  theme(legend.position = "right", 
        legend.text = element_text(angle = 25)) +
  guides(fill = "none"); map_low

# Combine these map panels in an intuitive way
cowplot::plot_grid(map_high, map_low, labels = "AUTO", ncol = 1)

# Save map
ggsave(filename = file.path("graphs", "map_global.jpg"),
       plot = last_plot(), width = 10, height = 5, units = "in", dpi = 560)

## ------------------------------------------ ##
        # Site Map Creation (V2) ----
## ------------------------------------------ ##
# Make a zoomed in low latitude map

#set new coordinates
lo_lats <- dplyr::filter(site_df, lat < 0)
lo_lims <- list("lat" = c(-79, -77), "lon" = c(155, 173))

# Create low latitude map
map_lo <- core_map +
  coord_sf(xlim = lo_lims[["lon"]], ylim = lo_lims[["lat"]], expand = F) +
  geom_point(data = lo_lats, aes(x = lon, y = lat, color = mean_si, 
                                 size = drainSqKm), shape = 19, alpha = 0.75) +
  scale_x_continuous(limits = lo_lims[["lon"]], 
                     breaks = seq(from = min(lo_lims[["lon"]]), 
                                  to = max(lo_lims[["lon"]]), 
                                  by = 10)) + 
  scale_y_continuous(limits = low_lims[["lat"]], 
                     breaks = seq(from = min(lo_lims[["lat"]]+1), 
                                  to = max(lo_lims[["lat"]]), 
                                  by = 2)) + 
  scale_fill_gradient(low = "#8ecae6", high = "#8ecae6", na.value = "transparent") +
  scale_color_gradient(low = "#780116", high = "#f7b538") +
  scale_size_binned(breaks = c(10^3, 10^6)) +
  # Put legend above map
  theme(legend.position = "below"); map_lo

# Save map - why isn't this saving correctly - dots are enormous
ggsave(filename = file.path("graphs", "map_mcmurdo.jpg"),
       plot = last_plot(), width = 6, height = 5, units = "in", dpi = 560)

# Clean up environment and collect garbage to speed R up going forward
rm(list = ls()); gc()

# End ----
