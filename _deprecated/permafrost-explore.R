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
    # Permafrost Extent (1km raster) ----
## ------------------------------------------ ##
# Name: Permafrost Extent and Ground Temperature Map, 2000-2016, Northern Hemisphere Permafrost
## Source: https://apgc.awi.de/dataset/pex

# Read in the raster
pf_rast <- terra::rast(x = file.path("permafrost_test", "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH", "UiO_PEX_PERPROB_5.0_20181128_2000_2016_NH.tif"))

# Check layers
names(pf_rast)

# Check structure
print(pf_rast)

# Re-project into desired CRS
## Note takes ~5 minutes to complete
pf_wgs84 <- terra::project(x = pf_rast, y = "epsg:4326")

## Export this locally for easier re-use
# terra::writeRaster(x = pf_wgs84, overwrite = T, filename = file.path("permafrost_test", "perprob.tif"))

# Exploratory plot (of permafrost probability)
terra::plot(pf_wgs84, axes = T)

# Re-clear environment
rm(list = ls())

## ------------------------------------------ ##
    # Permafrost Extent (25km netCDF) ----
## ------------------------------------------ ##
# Name: Permafrost Extent and Ground Temperature Map, 2000-2016, Northern Hemisphere Permafrost
## Source: https://apgc.awi.de/dataset/pex
## NetCDF Resampled from 1km to 25km resolution

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
## Looks like projection is wrong but the hard-coded one is the only one listed in the data doc

# Re-clear environment
rm(list = ls())

## ------------------------------------------ ##
     # Circum-Arctic Map of Permafrost ----
## ------------------------------------------ ##
# Name: Circum-Arctic Map of Permafrost and Ground-Ice Conditions, Version 2
## Source: https://nsidc.org/data/ggd318/versions/2

# Read in this file
pf_sf <- sf::st_read(dsn = file.path("permafrost_test", "permaice.shp"))

# Check various structural elements
sf::st_crs(x = pf_sf)

# Exploratory plot
## NOTE: waited 10 minutes and it doesn't work (plus crashed local RStudio)
## Need to explore alternate options for visualization
# plot(pf)

# Re-clear environment
rm(list = ls())

# End ----
