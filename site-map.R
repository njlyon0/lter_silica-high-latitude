## ------------------------------------------ ##
          # High Latitude Site Map
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Create publication-quality site map figure

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, readxl, sf, maps, terra, stars, supportR, cowplot)

# Clear environment
rm(list = ls())

# This map is very data hungry so we'll need to do garbage collection too
gc()

# Create a folder to store necessary files (if it doesn't already exist)
dir.create(path = file.path("map_data"), showWarnings = F)

# Identify needed files
(wanted_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>% 
    dplyr::bind_rows(googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1V5EqmOlWA8U9NWfiBcWdqEH9aRAP-zCk"))) %>% 
    # Filter to only desired files
    dplyr::filter(name %in% c("Site_Reference_Table")))

# Download those files
purrr::walk2(.x = wanted_files$id, .y = wanted_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("map_data", .y)))

# Read in those files
ref_v0 <- readxl::read_excel(path = file.path("map_data", "Site_Reference_Table.xlsx"))

# Read in one of the SiZer outputs
## Note this assumes that one of the `...-workflow.R` scripts has been run
sizer_outs <- read.csv(file = file.path("sizer_outs", "annual_Conc_uM_DSi_bw5.csv"))

# Wrangle the reference table to only the bits that we need
ref_v1 <- ref_v0 %>% 
  # Keep only a few columns
  dplyr::select(LTER, Stream_Name, Latitude, Longitude) %>% 
  # Pare down to only unique rows
  dplyr::distinct() %>% 
  # Simplify river names slightly
  dplyr::mutate(site_simp = gsub(pattern = " at", replacement = " ", x = Stream_Name)) %>%
  # Create a column that combines LTER and stream names
  dplyr::mutate(LTER_abbrev = ifelse(nchar(LTER) > 4,
                                     yes = stringr::str_sub(string = LTER, start = 1, end = 4),
                                     no = LTER),
                site_abbrev = ifelse(nchar(site_simp) > 14,
                                     yes = stringr::str_sub(string = site_simp, start = 1, end = 14),
                                     no = site_simp),
                stream = paste0(LTER_abbrev, "_", site_abbrev), .after = Stream_Name) %>% 
  # Drop intermediary columns
  dplyr::select(-dplyr::ends_with("_abbrev"), -site_simp) %>% 
  # Rename some of those
  dplyr::rename(lat = Latitude, lon = Longitude) %>% 
  # Filter to only streams in the SiZer outputs
  dplyr::filter(stream %in% unique(sizer_outs$stream))

# Check for mismatch between the two
supportR::diff_check(old = unique(sizer_outs$stream), new = unique(ref_v1$stream))

# Final pre-map creation steps
site_df <- sizer_outs %>% 
  # Combine the sizer outputs with the reference table
  dplyr::left_join(y = ref_v1, by = c("LTER", "Stream_Name", "stream")) %>% 
  # Group by key columns and calculate average silica concentration
  dplyr::group_by(LTER, Stream_Name, stream, drainSqKm, lat, lon) %>% 
  dplyr::summarize(mean_si = mean(Conc_uM, na.rm = T),
                   mean_abs_change = mean(abs(percent_change), na.rm = T)) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(site_df)

# Clean up environment
rm(list = setdiff(x = ls(), y = c("site_df")))

## ------------------------------------------ ##
          # Permafrost Wrangling ----
## ------------------------------------------ ##
# For more information on permafrost data see:
## https://apgc.awi.de/dataset/pex
## Note that I re-projected into WGS84 to match CRS of rivers

# Read in permafrost data
pf <- terra::rast(x = file.path("map_data", "permafrost-probability.tif"))

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
                   filename = file.path("map_data", "permafrost-simple.tif"))

# Clean up environment & collect garbage
rm(list = setdiff(x = ls(), y = c("site_df"))); gc()

# Read it back in as a stars object
pf_stars <- stars::read_stars(.x = file.path("map_data", "permafrost-simple.tif"))

# One final demo plot
ggplot() + 
  geom_stars(data = pf_stars, downsample = 10) +
  scale_fill_continuous(na.value = "transparent") +
  theme(legend.position = "none")

## ------------------------------------------ ##
              # Site Map Prep ----
## ------------------------------------------ ##

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

## ------------------------------------------ ##
            # Site Map Creation ----
## ------------------------------------------ ##

# Get around weird issue with scientific notation
options(scipen = 10000)

# Define the bits of the map that all sub-maps will want to re-use
core_map <-  borders %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  # Add permafrost to this section
  geom_stars(data = pf_stars, downsample = 10) +
  # Customize some global (ha ha) theme/formatting bits
  labs(x = "Longitude", y = "Latitude") +
  # scale_fill_continuous(na.value = "transparent") +
  supportR::theme_lyon() +
  theme(legend.position = "none")

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
                     breaks = seq(from = min(high_lims[["lat"]]), 
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

# Make a folder for these
dir.create(path = file.path("map_images"), showWarnings = F)

# Save map
ggsave(filename = file.path("map_images", "high-latitude_map.jpg"),
       plot = last_plot(), width = 10, height = 5, units = "in", dpi = 560)

# Clean up environment and collect garbage to speed R up going forward
rm(list = ls()); gc()

# End ----
