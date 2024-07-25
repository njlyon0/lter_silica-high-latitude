## ------------------------------------------------------- ##
                 # Download / Retrieve Data
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Identify & download data from Google Drive

# IMPORTANT CAVEAT:
## This script requires access to the "LTER-WG_Silica-Export" Shared Drive
## It will not work if you don't have prior access

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Clear environment
rm(list = ls())

# Make needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("map_data"), showWarnings = F)

## ----------------------------------------- ##
        # WRTDS Outputs Download ----
## ----------------------------------------- ##

# Identify the WRTDS outputs folder
wrtds_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1V5EqmOlWA8U9NWfiBcWdqEH9aRAP-zCk")) %>% 
  # And pare down the outputs to only desired ones
  dplyr::filter(name %in% c("Full_Results_WRTDS_annual.csv", "Full_Results_WRTDS_monthly.csv"))

# Check them
wrtds_files

# Download those files
purrr::walk2(.x = wrtds_files$id, .y = wrtds_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", .y)))

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
      # Spatial Drivers Download ----
## ----------------------------------------- ##

# Identify driver file(s)
driver_outs <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld")) %>% 
  # Filter to only desired file (omitting date stamp in file name for flexibility)
  dplyr::filter(stringr::str_detect(string = name, pattern = "all-data_si-extract_2_"))

# Check 'em
driver_outs

# Create a simplified file name (lacking date stamp)
driver_name <- "all-data_si-extract_2.csv"

# Download that file
purrr::walk2(.x = driver_outs$id, .y = driver_outs$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", driver_name)))

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
        # Reference Table Download ----
## ----------------------------------------- ##

# Identify ref. table file
ref_out <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
  dplyr::filter(name == "Site_Reference_Table")

# Check it
ref_out

# Download it
purrr::walk2(.x = ref_out$id, 
             .y = paste0(ref_out$name, ".csv"),
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", .y)))

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
      # Permafrost Raster Download ----
## ----------------------------------------- ##

# Identify ref. table file
pf_out <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1YwVpzL9cGINyadparC5L8NaHMZF3vPa4")) %>%
  dplyr::filter(name == "permafrost-probability.tif")

# Check it
pf_out

# Download it
purrr::walk2(.x = pf_out$id, .y = pf_out$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("map_data", .y)))

# Clear environment
rm(list = ls())

# End ----
