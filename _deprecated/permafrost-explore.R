## ------------------------------------------ ##
            # Permafrost Exploration
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Explore permafrost data options (before building them into the site map)

## ------------------------------------------ ##
            # Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, sf)

# Clear environment & collect garbage
rm(list = ls()); gc()

## ------------------------------------------ ##
     # Circum-Arctic Map of Permafrost ----
## ------------------------------------------ ##
# Name: Circum-Arctic Map of Permafrost and Ground-Ice Conditions, Version 2
## Source: https://nsidc.org/data/ggd318/versions/2

# Read in this file
pf <- sf::st_read(dsn = file.path("permafrost_test", "permaice.shp"))

# Check various structural elements
sf::st_crs(x = pf)

# Exploratory plot
## NOTE: takes a few minutes to load
plot(pf)








# End ----
