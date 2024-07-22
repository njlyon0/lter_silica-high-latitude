## ------------------------------------------------------- ##
                      # Ordinations
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# PURPOSE:
## Create ordinations (e.g., PCoA, PCA, NMS, etc.) of multivariate response dataframes
## Includes basin characteristics / climatic "drivers" extracted elsewhere

## ----------------------------------------- ##
            # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, cowplot, supportR, vegan, ape)

# Make a folder for local preservation of data / graphs
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)

# Download drivers / basin characteristics
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK")) %>%
  dplyr::filter(name == "all-data_si-extract.csv") %>%
  googledrive::drive_download(file = .$id, path = file.path("data", .$name), overwrite = T)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
                  # Data Prep ----
## ----------------------------------------- ##

# Read in the basin characteristics (downloaded above)
basin_char <- read.csv(file = file.path("data", "all-data_si-extract.csv")) %>%
  # Rename relevant stream name column to match with SiZer outputs
  dplyr::rename(site = Stream_Name) %>%
  # Drop possibly misleading name columns that won't match with SiZer outputs
  dplyr::select(-Discharge_Site_Name, -Shapefile_Name) %>%
  # Also drop non-numeric characteristics
  dplyr::select(-dplyr::contains("major"), -dplyr::starts_with("greenup_"))

# See options for characteristics with data
dplyr::glimpse(basin_char)

# Read in SiZer outputs for annual data
year_full <- read.csv(file = file.path("sizer_outs", "annual_Yield_kmol_yr_km2_DSi_bw5.csv")) %>%
  # Drop ARC streams
  dplyr::filter(LTER != "ARC") %>%
  # Join on the basin characteristics
  dplyr::left_join(y = basin_char, by = dplyr::join_by(LTER, site))

# Check structure
dplyr::glimpse(year_full)

# Do the same for seasonal data
seas_full <- read.csv(file = file.path("sizer_outs", "seasonal_Yield_kmol_yr_km2_DSi_bw5.csv")) %>%
  dplyr::filter(LTER != "ARC") %>%
  dplyr::left_join(y = basin_char, by = dplyr::join_by(LTER, site))

# Check structure
dplyr::glimpse(seas_full)

# Choose grouping columns and only desired basin characteristics / climatic drivers
year_actual <- year_full %>%
  dplyr::select(sizer_bandwidth:chemical, test_p_value, r_squared, slope_estimate,
                ## "Comment out" climatic groups that *are not* desired
                ## Unspecified comments are dropped implicitly
                dplyr::starts_with("snow_"),
                dplyr::starts_with("evapotrans_"),
                dplyr::starts_with("npp_"),
                dplyr::starts_with("precip_"),
                dplyr::starts_with("temp_"),
                dplyr::starts_with("land_"),
                dplyr::starts_with("soil_"),
                dplyr::starts_with("rocks_"),
                dplyr::starts_with("elevation_")) %>%
  # Need to drop McMurdo because we have no basin characteristic information for this LTER
  dplyr::filter(LTER != "MCM") %>%
  # Keep only non-missing slope estimates too
  dplyr::filter(is.na(slope_estimate) != TRUE) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check what remains
names(year_actual)

# Do the same for seasonal data
seas_actual <- seas_full %>%
  dplyr::select(sizer_bandwidth:chemical, test_p_value, r_squared, slope_estimate,
                ## "Comment out" climatic groups that *are not* desired
                ## Unspecified comments are dropped implicitly
                dplyr::starts_with("snow_"),
                dplyr::starts_with("evapotrans_"),
                dplyr::starts_with("npp_"),
                dplyr::starts_with("precip_"),
                dplyr::starts_with("temp_"),
                dplyr::starts_with("land_"),
                dplyr::starts_with("soil_"),
                dplyr::starts_with("rocks_"),
                dplyr::starts_with("elevation_")) %>%
  # Need to drop McMurdo because we have no basin characteristic information for this LTER
  dplyr::filter(LTER != "MCM") %>%
  # Keep only non-missing slope estimates too
  dplyr::filter(is.na(slope_estimate) != TRUE) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(seas_actual)

# Identify chemical / response variable for both temporal granularities
## Annual data
(year_chem <- unique(year_full$chemical))
(year_respvar <- names(year_full[8]))
## Seasonal data
(seas_chem <- unique(seas_full$chemical))
(seas_respvar <- names(seas_full[9]))

## ----------------------------------------- ##
            # Annual - PCoA ----
## ----------------------------------------- ##

# Strip off just the response columns
year_resp <- year_actual %>%
  dplyr::select(-sizer_bandwidth:-r_squared)

# Check structure
dplyr::glimpse(year_resp)

# Get distance matrix
year_dist <- vegan::vegdist(x = year_resp, method = "euclidean", na.rm = T)

# Perform principal coordinates analysis on this matrix
year_points <- ape::pcoa(D = year_dist)

# Generate a nice file name for this graph
(year_name <- paste0("annual_pcoa_", year_chem, "_", year_respvar, ".png"))

# Create (and export) an ordination of these data
png(file = file.path("graphs", year_name), width = 720, height = 720)

supportR::pcoa_ord(mod = year_points, groupcol = year_actual$LTER,
                   title = paste0("Annual PCoA of ", year_chem, " Time Series Slope and Basin Characteristics"),
                   leg_pos = "bottomright", pt_size = 2)

dev.off()

## ----------------------------------------- ##
            # Seasonal - PCoA ----
## ----------------------------------------- ##

# Do the same thing but this time, we'll loop across the seasons
for(focal_season in unique(seas_actual$season)){
  
  # Print starting message
  message("Making ordination for season: ", focal_season)
  
  # Filter to only that season
  seas_sub <- seas_actual %>%
    dplyr::filter(season == focal_season)
  
  # Generate a version with only response columns
  seas_resp <-  seas_sub %>%
    dplyr::select(-sizer_bandwidth:-r_squared)
  
  # Get distance matrix
  seas_dist <- vegan::vegdist(x = seas_resp, method = "euclidean", na.rm = T)
  
  # Perform principal coordinates analysis on this matrix
  seas_points <- ape::pcoa(D = seas_dist)
  
  # Generate a nice file name for this graph
  seas_name <- paste0("seasonal_pcoa_", toupper(focal_season), "_", seas_chem, 
                      "_", seas_respvar, ".png")
  
  # Create (and export) an ordination of these data
  png(file = file.path("graphs", seas_name), width = 720, height = 720)
  
  supportR::pcoa_ord(mod = seas_points, groupcol = seas_sub$LTER,
                     title = paste0("Seasonal PCoA of ", seas_chem, " Time Series Slope and Basin Characteristics"),
                     leg_pos = "bottomright", pt_size = 2)
  
  dev.off() }


# End ----
