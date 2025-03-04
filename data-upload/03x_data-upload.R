## ------------------------------------------------------- ##
# Upload 03 Products to Google Drive
## ------------------------------------------------------- ##
# Written by: Nick J Lyon, 

# Purpose:
## Upload data produced by script with same number prefix (e.g., "01_") to Google Drive

# Pre-Requisites:
## This script assumes you've run the corresponding "01_" script in the top-level of the repository

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ----------------------------------------- ##
# Upload Stats Results (3A) ----
## ----------------------------------------- ##

# Define annual path
stat_path <- file.path("data", "stats-results")

# Define annual Drive folder
stat_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1t7s3bZcHHjEXa8tvup6zK5erDpmle88l")

# If annual outputs exist:
if(dir.exists(stat_path)){
  
  # Identify local annual files
  stat_local <- dir(path = stat_path)
  
  # Iterate across them uploading each
  purrr::walk(.x = stat_local,
              .f = ~ googledrive::drive_upload(media = file.path(stat_path, .x), 
                                               overwrite = T, path = stat_url))
  
}

# End ----
