## ------------------------------------------ ##
        # Synchrony Figure Preparation
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Create publication-quality site map figure

## ------------------------------------------ ##
              # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, sf, maps, terra, njlyon0/supportR, cowplot)

# Clear environment
rm(list = ls())

# This map is very data hungry so we'll need to do garbage collection too
gc()

# Create a folder to store necessary files (if it doesn't already exist)
dir.create(path = file.path("map_data"), showWarnings = F)

# Identify names of files this script requires
coord_file <- "lter_site_coordinates.csv"
landcover_file <- "gblulcgeo20.tif"

# Check whether we already have the files
(ready_files <- dir(path = file.path("map_data")))

# Identify the files in our desired Drive folder
(map_files <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1wo2xocmHx0isWwp3siX85rTFB9TvfnNx")) %>%
  # Filter to only desired files
  dplyr::filter(name %in% c(coord_file, landcover_file)) %>%
  # And further remove any files we already have downloaded locally
    ## Really want to avoid re-downloading the .tif if at all possible
  dplyr::filter(!name %in% ready_files))

# Download files we don't already have into the folder we have for them
purrr::walk2(.x = map_files$id, 
             .y = map_files$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("map_data", .y),
                                                overwrite = T))

# Gather up some needed plotting aesthetics
# Site palette
site_palette <- c("CWT" = "#bd0026", "LUQ" = "orange", "HBR" = "gold", 
                  "AND" = "limegreen", "CDR" = "lightblue", "BNZ" = "#f1b6da", 
                  "SEV" = "#9d4edd")
# Define shape palette
shp_palette <- c("AND" = 22, "BNZ" = 21, "CDR" = 24, "CWT" = 23, 
                 "HBR" = 22, "LUQ" = 21, "SEV" = 24)

# Clean up environment
rm(list = setdiff(ls(), c("site_palette", "shp_palette")))

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
