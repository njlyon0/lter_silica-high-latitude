## ------------------------------------------------------- ##
# Upload 04 Products to Google Drive
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
# Upload Figures ----
## ----------------------------------------- ##

# Define path
fig_path <- file.path("graphs", "figures")

# Define Drive folder
fig_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/11Ya520cy7IRyUAmRhtrk0C4FgcjtnH4t")

# If outputs exist:
if(dir.exists(fig_path)){
  
  # Identify local files
  fig_local <- dir(path = fig_path)
  
  # Iterate across them uploading each
  purrr::walk(.x = fig_local,
              .f = ~ googledrive::drive_upload(media = file.path(fig_path, .x), 
                                               overwrite = T, path = fig_url))
  
}

# End ----
