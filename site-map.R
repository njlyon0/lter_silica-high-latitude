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
librarian::shelf(googledrive, tidyverse, purrr, readxl, sf, maps, terra, supportR, cowplot)

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
              # Permafrost Prep ----
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

# Crop off the unneeded latitudes
pf_v4 <- terra::crop(x = pf_v3, y = c(-180, 180, 15, 90))

# One more demo plot
plot(pf_v4, axes = T, 
     main = paste0("Permafrost probability ≥", (pf_thresh * 100), "% (set to 1)"))

# Make an empty list
pf_list <- list()

# Loop across some longitude ranges
for(rng in c((60 * 0:2))){
  
  # Identify full range
  span <- sort(c(rng, rng + 60))
  
  # Identify the negative span too
  neg_span <- sort(span * -1)
  
  # Crop the raster to its positive and negative spans
  pf_crop_pos <- terra::crop(x = pf_v4, y = terra::ext(x = c(span, 15, 90)))
  pf_crop_neg <- terra::crop(x = pf_v4, y = terra::ext(x = c(neg_span, 15, 90)))
  
  # Coerce both to dataframes
  pf_pos_df <- as.data.frame(pf_crop_pos, xy = T)
  pf_neg_df <- as.data.frame(pf_crop_neg, xy = T)
  
  # Bind them together
  span_df <- dplyr::bind_rows(pf_pos_df, pf_neg_df)
  
  # Add to the list
  pf_list[[as.character(rng)]] <- span_df
  
  # End with a processing message
  message("Processing complete for '", paste0(span, collapse = " to "), 
          "' and '", paste0(neg_span, collapse = " to "), "'") }

# Unlist to a dataframe
pf_df <- purrr::list_rbind(x = pf_list)

# Check structure
dplyr::glimpse(pf_df)

# Clean up environment & collect garbage
rm(list = setdiff(x = ls(), y = c("site_df", "pf_df"))); gc()

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
high_lats <- dplyr::filter(site_df, lat > 0)

# Check range of lat/long for all limits
range(e_lats$lat); range(e_lats$lon)
range(w_lats$lat); range(w_lats$lon)
range(lo_lats$lat); range(lo_lats$lon)
range(high_lats$lat); range(high_lats$lon)

# Define borders of maps so that the site points will be nicely inside the borders
e_lims <- list("lat" = c(55, 75), "lon" = c(0, 170))
w_lims <- list("lat" = c(55, 75), "lon" = c(-170, -135))
low_lims <- list("lat" = c(-80, -60), "lon" = c(-180, 180)) #zoomed out
lo_lims <- list("lat" = c(-80, -70), "lon" = c(150, 180)) #zoomed in
high_lims <- list("lat" = c(55, 80), "lon" = c(-170, 170)) 

## ------------------------------------------ ##
            # Site Map Creation ----
## ------------------------------------------ ##

# Define the bits of the map that all sub-maps will want to re-use
core_map <-  borders %>% 
  ggplot() +
  geom_sf(fill = "gray95") +
  # Add permafrost to this section
  geom_tile(data = pf_df, aes(x = x, y = y), col = "purple", alpha = 0.5) +
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

# Make the low latitude map - zoomed out
map_low <- core_map +
  # Set map extent
  coord_sf(xlim = low_lims[["lon"]], ylim = lo_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aethetics
  geom_point(data = lo_lats, aes(x = lon, y = lat, fill = mean_si, size = drainSqKm), shape = 21) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = low_lims[["lon"]], 
                     breaks = seq(from = min(low_lims[["lon"]]), 
                                  to = max(low_lims[["lon"]]), 
                                  by = 50)) + 
  scale_y_continuous(limits = low_lims[["lat"]], 
                     breaks = seq(from = min(low_lims[["lat"]]), 
                                  to = max(low_lims[["lat"]]), 
                                  by = 15)) + 
  # Remove legend (for now)
  theme(legend.position = "below"); map_low

# Make the low latitude map - zoomed in
map_lo <- core_map +
  # Set map extent
  coord_sf(xlim = lo_lims[["lon"]], ylim = lo_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aethetics
  geom_point(data = lo_lats, aes(x = lon, y = lat, fill = mean_si, size = drainSqKm), shape = 21) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = lo_lims[["lon"]], 
                     breaks = seq(from = min(lo_lims[["lon"]]), 
                                  to = max(lo_lims[["lon"]]), 
                                  by = 10)) + 
  scale_y_continuous(limits = lo_lims[["lat"]], 
                     breaks = seq(from = min(lo_lims[["lat"]]), 
                                  to = max(lo_lims[["lat"]]), 
                                  by = 10)) + 
  # Remove legend (for now)
  theme(legend.position = "below"); map_lo

# Make the high latitude all long map
map_high <- core_map +
  # Set map extent
  coord_sf(xlim = high_lims[["lon"]], ylim = high_lims[["lat"]], expand = F) +
  # Add points for sites and customize point aethetics
  geom_point(data = high_lats, aes(x = lon, y = lat, fill = mean_si, size = drainSqKm), shape = 21) +
  # Make axis tick marks nice and neat
  scale_x_continuous(limits = high_lims[["lon"]], 
                     breaks = seq(from = min(high_lims[["lon"]]), 
                                  to = max(high_lims[["lon"]]), 
                                  by = 50)) + 
  scale_y_continuous(limits = high_lims[["lat"]], 
                     breaks = seq(from = min(high_lims[["lat"]]), 
                                  to = max(high_lims[["lat"]]), 
                                  by = 15)) + 
  # Remove legend (for now)
  theme(legend.position = "bottom"); map_high

# Combine these map panels in an intuitive way
## Need to experiment more with this...
cowplot::plot_grid(cowplot::plot_grid(map_high, labels = c("A", nrow = 1)),
                   map_low, labels = c("", "B"), ncol = 1)

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
