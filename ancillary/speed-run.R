## ------------------------------------------------------- ##
                    # "Speed Run" Script
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# Purpose:
## Sometimes you just want to rapidly re-process inputs for the SiZer workflow or the statistics preparation script
## This script allows you (with minor modifications to 01 and 02) to quickly do just that

# WARNING:
## While this script does require you to edit 01 and/or 02, you SHOULD NOT COMMIT THOSE CHANGES
## This script is a convenience but the edits is requires **will break 01 and 02 when run as they were intended to run**

# Pre-Requisites:
## This script assumes you've done one of the following options:
### A) run the "00_data-download.R" script
### B) manually downloaded any needed inputs and put them in a folder named "data"

## ----------------------------------------- ##
# Speed Run: Core Workflow (01) ----
## ----------------------------------------- ##

# For this to work, you need to comment out the following lines in the 01 script
## line 27 (environment clearing)
## line 133 (element definition)

# DO NOT COMMIT THOSE CHANGES
## Comment out those lines, save the file, run this script, then delete the '#' and re-save the file

# Loop across all available elements
for(element in c("DSi", "DIN", "P", "Si_DIN", "Si_P")){
  
  # Loop across script
  source(file.path("01_sizer-workflow.R"))
  
  # Clear environment / collect garbage
  rm(list = ls()); gc()
}

## ----------------------------------------- ##
# Speed Run: Stats-Prep (02) ----
## ----------------------------------------- ##

# For this to work, you need to comment out the following lines in the 02 script
## line 22 (environment clearing)
## line 33-35 (defining of input file object)

# DO NOT COMMIT THOSE CHANGES
## Comment out those lines, save the file, run this script, then delete the '#' and re-save the file

# Identify relevant files
for(sizer_file in dir(path = file.path("data"), pattern = "sizer-outs_annual_")){
  
  # Loop across 'stats prep' script
  source(file.path("02_stats-prep.R"))
  
  # Clear environment / collect garbage
  rm(list = ls()); gc()
}

# End ----
