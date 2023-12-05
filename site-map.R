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
librarian::shelf(googledrive, tidyverse, readxl, sf, maps, terra, supportR, cowplot)

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
  dplyr::select(-dplyr::ends_with("_abbrev")) %>% 
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
# Land Cover Prep ----
## ------------------------------------------ ##

# Read in land cover raster
# lulc <- terra::rast(x = file.path("map_data", "gblulcgeo20.tif"))

# Crop to only North America
# lulc_crop <- terra::crop(x = lulc, y = terra::ext(c(lon_lims, lat_lims)))

# Demo plot to be sure that worked
# plot(lulc_crop, axes = T)

# Keep only tree categories
# lc_v2 <- (lulc_crop %in% c(11:15)) # True forest only
# lc_v2B <- (lulc_crop %in% c(6, 11:15, 18, 21)) # Forest + wooded areas
# plot(lc_v2, axes = T)
# plot(lc_v2B, axes = T)
## 6: cropland/woodland mosaic
## 11: deciduous broadleaf forest
## 12: deciduous needleleaf forest
## 13: evergreen broadleaf forest
## 14: evergreen needleleaf forest
## 15: mixed forest
## 18: wooded wetland
## 21: wooded tundra

# Coerce the raster into a dataframe with one row per XY coordinate
# lc_v3 <- as.data.frame(lc_v2, xy = T) %>%
#   # Retain only true content pixels
#   dplyr::filter(gblulcgeo20 != 0)
# 
# # Check that out
# dplyr::glimpse(lc_v3)

## ------------------------------------------ ##
              # Site Map Prep ----
## ------------------------------------------ ##

# Get country & US state borders
borders <- sf::st_as_sf(maps::map(database = "world", plot = F, fill = T)) %>%
  dplyr::bind_rows(sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)))

# Split our site information into low latitudes vs E & W high latitudes
e_lats <- dplyr::filter(site_df, lat > 0 & lon > 0)
w_lats <- dplyr::filter(site_df, lat > 0 & lon < 0)
lo_lats <- dplyr::filter(site_df, lat < 0)

# Check range of lat/long for all limits
range(e_lats$lat); range(e_lats$lon)
range(w_lats$lat); range(w_lats$lon)
range(lo_lats$lat); range(lo_lats$lon)

# Define borders of maps so that the site points will be nicely inside the borders
e_lims <- list("lat" = c(55, 75), "lon" = c(0, 170))
w_lims <- list("lat" = c(55, 75), "lon" = c(-170, -135))
lo_lims <- list("lat" = c(-80, -60), "lon" = c(135, 180)) 

## ------------------------------------------ ##
            # Site Map Creation ----
## ------------------------------------------ ##

# Define the bits of the map that all sub-maps will want to re-use
core_map <-  borders %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  # Add land cover to this section
  # geom_tile(data = ..., aes(x = x, y = y), col = "purple", alpha = 0.5) +
  # Customize some global (ha ha) theme/formatting bits
  labs(x = "Longitude", y = "Latitude") +
  supportR::theme_lyon()

# Make the high latitude Eastern map
map_e <- core_map +
  # Set map extent
  coord_sf(xlim = e_lims[["lon"]], ylim = e_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aethetics
  geom_point(data = e_lats, aes(x = lon, y = lat, fill = mean_si, size = drainSqKm), shape = 21) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = e_lims[["lon"]], 
                     breaks = seq(from = min(e_lims[["lon"]]), 
                                  to = max(e_lims[["lon"]]), 
                                  by = 25)) + 
  scale_y_continuous(limits = e_lims[["lat"]], 
                     breaks = seq(from = min(e_lims[["lat"]]), 
                                  to = max(e_lims[["lat"]]), 
                                  by = 15)) + 
  # Remove legend (for now)
  theme(legend.position = "none"); map_e

# Make the high latitude Western map
map_w <- core_map +
  # Set map extent
  coord_sf(xlim = w_lims[["lon"]], ylim = w_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aethetics
  geom_point(data = w_lats, aes(x = lon, y = lat, fill = mean_si, size = drainSqKm), shape = 21) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = w_lims[["lon"]], 
                     breaks = seq(from = min(w_lims[["lon"]]), 
                                  to = max(w_lims[["lon"]]), 
                                  by = 25)) + 
  scale_y_continuous(limits = w_lims[["lat"]], 
                     breaks = seq(from = min(w_lims[["lat"]]), 
                                  to = max(w_lims[["lat"]]), 
                                  by = 15)) + 
  # Remove legend (for now)
  theme(legend.position = "none"); map_w

# Make the low latitude map
map_lo <- core_map +
  # Set map extent
  coord_sf(xlim = lo_lims[["lon"]], ylim = lo_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aethetics
  geom_point(data = lo_lats, aes(x = lon, y = lat, fill = mean_si, size = drainSqKm), shape = 21) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = lo_lims[["lon"]], 
                     breaks = seq(from = min(lo_lims[["lon"]]), 
                                  to = max(lo_lims[["lon"]]), 
                                  by = 25)) + 
  scale_y_continuous(limits = lo_lims[["lat"]], 
                     breaks = seq(from = min(lo_lims[["lat"]]), 
                                  to = max(lo_lims[["lat"]]), 
                                  by = 15)) + 
  # Remove legend (for now)
  theme(legend.position = "none"); map_lo

# Combine these map panels in an intuitive way
## Need to experiment more with this...
cowplot::plot_grid(cowplot::plot_grid(map_w, map_e, labels = c("A", "B", nrow = 1)),
                   map_lo, labels = c("", "C"), ncol = 1)

# Make a folder for these
dir.create(path = file.path("map_images"), showWarnings = F)

# Save each panel locally
## East
ggsave(filename = file.path("map_images", "northeast_map.png"),
       plot = map_e, width = 8, height = 6, units = "in", dpi = 560)
## West
ggsave(filename = file.path("map_images", "northwest_map.png"),
       plot = map_w, width = 6, height = 6, units = "in", dpi = 560)
## South
ggsave(filename = file.path("map_images", "south_map.png"),
       plot = map_lo, width = 6, height = 6, units = "in", dpi = 560)

# Clean up environment and collect garbage to speed R up going forward
rm(list = ls())
gc()

# End ----
