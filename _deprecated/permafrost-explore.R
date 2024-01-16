## ------------------------------------------ ##
            # Permafrost Exploration
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## Explore permafrost data options (before building them into the site map)

## ------------------------------------------ ##
            # Housekeeping ----
## ------------------------------------------ ##
# Clear environment & collect garbage
rm(list = ls()); gc()

# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse, sf, ncdf4, terra)

## ------------------------------------------ ##
    # Permafrost Extent & Ground Temp ----
## ------------------------------------------ ##
# Name: Permafrost Extent and Ground Temperature Map, 2000-2016, Northern Hemisphere Permafrost
## Source: https://apgc.awi.de/dataset/pex

# Read in this file
pf_nc <- ncdf4::nc_open(filename = file.path("permafrost_test", "UiO_PEX_5.0_20181127_2000_2016_25km.nc"))

# Check out some of its structure
print(pf_nc)

# Read in as a raster too
pf_rast <- terra::rast(x = file.path("permafrost_test", "UiO_PEX_5.0_20181127_2000_2016_25km.nc"))

# Check layers
names(pf_rast)

# Check structure of one layer
print(pf_rast$PerProb)

# Split that off into its own thing
pf_prob <- pf_rast[["PerProb"]]

# Define initial CRS (Arctic Polar Stereographic)
terra::crs(x = pf_prob) <- "epsg:3995"

# Re-project into desired CRS
pf_wgs84 <- terra::project(x = pf_prob, y = "epsg:4326")

# Exploratory plot (of permafrost probability)
terra::plot(pf_wgs84, axes = T)


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
## NOTE: waited 10 minutes and it doesn't work (plus crashed local RStudio)
## Need to explore alternate options for visualization
# plot(pf)


# End ----
