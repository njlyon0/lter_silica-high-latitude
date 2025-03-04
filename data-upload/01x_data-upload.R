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
# Upload Annual Data ----
## ----------------------------------------- ##

# Define annual path
ann_path <- file.path("data", "sizer-outs_annual")

# Define annual Drive folder
ann_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1000ftGmNpSQpMWM4WZj6DmoJHfZbJNph")

# If annual outputs exist:
if(dir.exists(ann_path)){
  
  # Identify local annual files
  ann_local <- dir(path = ann_path)
  
  # Iterate across them uploading each
  purrr::walk(.x = ann_local,
              .f = ~ googledrive::drive_upload(media = file.path(ann_path, .x), 
                                               overwrite = T, path = ann_url))
  
}

## ----------------------------------------- ##
# Upload Monthly Data ----
## ----------------------------------------- ##

# Define annual path
mon_path <- file.path("data", "sizer-outs_monthly")

# Define annual Drive folder
mon_url <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1flrtmNTQ9ZVwcblS-S1_sB6IWTNovJQ4")

# If annual outputs exist:
if(dir.exists(mon_path)){
  
  # Identify local annual files
  mon_local <- dir(path = mon_path)
  
  # Iterate across them uploading each
  purrr::walk(.x = mon_local,
              .f = ~ googledrive::drive_upload(media = file.path(mon_path, .x), 
                                               overwrite = T, path = mon_url))
  
}

# End ----
