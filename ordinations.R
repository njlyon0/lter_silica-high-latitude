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
librarian::shelf(tidyverse, googledrive, cowplot, supportR)

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
  dplyr::select(sizer_bandwidth:Year, test_p_value, r_squared, slope_estimate,
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
                dplyr::starts_with("elevation_"))

# Check what remains
names(year_actual)

# Do the same for seasonal data
seas_actual <- seas_full %>%
  dplyr::select(sizer_bandwidth:Year, test_p_value, r_squared, slope_estimate,
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
                dplyr::starts_with("elevation_"))

# Check it out
dplyr::glimpse(seas_actual)

## ----------------------------------------- ##
#  ----
## ----------------------------------------- ##





# End ----
