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
  # Generate a 'LTER + stream' column
  dplyr::mutate(stream = paste0(LTER, "_", Stream_Name, .after = Stream_Name)) %>% 
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
              # Site Map Prep ----
## ------------------------------------------ ##

# Get country & US state borders
borders <- sf::st_as_sf(maps::map(database = "world", plot = F, fill = T)) %>%
  dplyr::bind_rows(sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)))

# Split our site information into high vs. low latitudes
hi_lats <- dplyr::filter(site_df, lat > 0)
lo_lats <- dplyr::filter(site_df, lat < 0)

# Check range of lat/long for both
range(hi_lats$lat); range(hi_lats$lon)
range(lo_lats$lat); range(lo_lats$lon)

# Define borders of map so that the site points will be nicely inside the borders
lims <- list( "high" = list("lat" = c(55, 75), "lon" = c(-152, 165)),
              "low" = list("lat" = c(-70, -80), "lon" = c(155, 165)) )


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
# Site Map Creation ----
## ------------------------------------------ ##

# Make map figure!
fig1c <- combo %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  # Set map extent
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = F) +
  # Add forest cover information
  geom_tile(data = lc_v3, aes(x = x, y = y), col = "darkolivegreen4", alpha = 0.5) +
  # Add points for LTERs (and customize their aesthetics)
  geom_point(data = lter_coords, aes(x = lon, y = lat, 
                                     fill = LTER, shape = LTER), size = 4) +
  scale_fill_manual(values = site_palette) +
  scale_shape_manual(values = shp_palette) + # different shapes for each LTER
  # Add labels for LTERs
  geom_label(data = lter_coords, aes(x = lon, y = lat),
             label = lter_coords$LTER, nudge_y = 3, size = 3, fontface = "bold", 
             label.padding = unit(x = 0.15, units = "lines")) +
  # Tweak theme / formatting
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = lon_lims, breaks = seq(from = -150, to = -75, by = 25)) + 
  scale_y_continuous(limits = lat_lims, breaks = seq(from = 20, to = 70, by = 15)) +
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, vjust = 1, hjust = 0.5)); fig1c

# Add label on the map
cowplot::plot_grid(fig1c, labels = c("C"), nrow = 1)

# Make sure that the figure folder exists
dir.create(path = file.path("synchrony_figure_files"), showWarnings = F)

# Name the map file
map_name <- "sync_fig1C_map.png"

# Save locally
ggsave(filename = file.path("synchrony_figure_files", map_name),
       plot = last_plot(), width = 6, height = 6, units = "in", dpi = 420)

# Upload to Google Drive
googledrive::drive_upload(media = file.path("synchrony_figure_files", map_name),
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1wZqCP-axj9KUfAaiPJsTamc03zsgngCY"),
                          overwrite = T)

# Clean up environment and collect garbage to speed R up going forward
rm(list = ls())
gc()





## ------------------------------------------ ##
              # Site Map Prep ----
## ------------------------------------------ ##

# Combine global and state map
combo <- sf::st_as_sf(maps::map(database = "world", plot = F, fill = T)) %>%
  dplyr::bind_rows(sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)))

# Read in LTER site coordinates
(lter_coords <- read.csv(file.path("map_data", "lter_site_coordinates.csv")))

# Set lat/long limits
lat_lims <- c(15, 75)
lon_lims <- c(-160, -60)

# Read in land cover raster
lulc <- terra::rast(x = file.path("map_data", "gblulcgeo20.tif"))

# Crop to only North America
lulc_crop <- terra::crop(x = lulc, y = terra::ext(c(lon_lims, lat_lims)))

# Demo plot to be sure that worked
plot(lulc_crop, axes = T)

# Keep only tree categories
lc_v2 <- (lulc_crop %in% c(11:15)) # True forest only
lc_v2B <- (lulc_crop %in% c(6, 11:15, 18, 21)) # Forest + wooded areas
plot(lc_v2, axes = T)
plot(lc_v2B, axes = T)
## 6: cropland/woodland mosaic
## 11: deciduous broadleaf forest
## 12: deciduous needleleaf forest
## 13: evergreen broadleaf forest
## 14: evergreen needleleaf forest
## 15: mixed forest
## 18: wooded wetland
## 21: wooded tundra

# Coerce the raster into a dataframe with one row per XY coordinate
lc_v3 <- as.data.frame(lc_v2, xy = T) %>%
  # Retain only true content pixels
  dplyr::filter(gblulcgeo20 != 0)

# Check that out
dplyr::glimpse(lc_v3)

## ------------------------------------------ ##
            # Site Map Creation ----
## ------------------------------------------ ##

# Make map figure!
fig1c <- combo %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  # Set map extent
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = F) +
  # Add forest cover information
  geom_tile(data = lc_v3, aes(x = x, y = y), col = "darkolivegreen4", alpha = 0.5) +
  # Add points for LTERs (and customize their aesthetics)
  geom_point(data = lter_coords, aes(x = lon, y = lat, 
                                     fill = LTER, shape = LTER), size = 4) +
  scale_fill_manual(values = site_palette) +
  scale_shape_manual(values = shp_palette) + # different shapes for each LTER
  # Add labels for LTERs
  geom_label(data = lter_coords, aes(x = lon, y = lat),
             label = lter_coords$LTER, nudge_y = 3, size = 3, fontface = "bold", 
             label.padding = unit(x = 0.15, units = "lines")) +
  # Tweak theme / formatting
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(limits = lon_lims, breaks = seq(from = -150, to = -75, by = 25)) + 
  scale_y_continuous(limits = lat_lims, breaks = seq(from = 20, to = 70, by = 15)) +
  supportR::theme_lyon() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, vjust = 1, hjust = 0.5)); fig1c

# Add label on the map
cowplot::plot_grid(fig1c, labels = c("C"), nrow = 1)

# Make sure that the figure folder exists
dir.create(path = file.path("synchrony_figure_files"), showWarnings = F)

# Name the map file
map_name <- "sync_fig1C_map.png"

# Save locally
ggsave(filename = file.path("synchrony_figure_files", map_name),
       plot = last_plot(), width = 6, height = 6, units = "in", dpi = 420)

# Upload to Google Drive
googledrive::drive_upload(media = file.path("synchrony_figure_files", map_name),
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1wZqCP-axj9KUfAaiPJsTamc03zsgngCY"),
                          overwrite = T)

# Clean up environment and collect garbage to speed R up going forward
rm(list = ls())
gc()

# End ----
