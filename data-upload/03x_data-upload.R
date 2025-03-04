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

# Define path
stat_path <- file.path("data", "stats-results")

# Define Drive folder
stat_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1t7s3bZcHHjEXa8tvup6zK5erDpmle88l")

# If outputs exist:
if(dir.exists(stat_path)){
  
  # Identify local files
  stat_local <- dir(path = stat_path)
  
  # Iterate across them uploading each
  purrr::walk(.x = stat_local,
              .f = ~ googledrive::drive_upload(media = file.path(stat_path, .x), 
                                               overwrite = T, path = stat_url))
  
}

## ----------------------------------------- ##
# Upload Exploratory Graphs (3B) ----
## ----------------------------------------- ##

# Skipping for now!
## Because these graphs are purely for exploration it is okay for them to exist only locally

## ----------------------------------------- ##
# Upload Site Map Files (3C) ----
## ----------------------------------------- ##

# Define path
map_path <- file.path("graphs")

# Define Drive folder
map_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/104dMyX9VES2vup8OQyvG1SatUqDPi5W0")

# If outputs exist:
if(dir.exists(map_path)){
  
  # Identify local files
  map_local <- dir(path = map_path, pattern = "map_")
  
  # Iterate across them uploading each
  purrr::walk(.x = map_local,
              .f = ~ googledrive::drive_upload(media = file.path(map_path, .x), 
                                               overwrite = T, path = map_url))
  
}

# End ----
