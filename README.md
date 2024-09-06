# High Latitude Silica Exports

- Primary Investigators: Joanna Carey & Kathi Jo Jankowski
- [Project Summary](https://lternet.edu/working-groups/river-si-exports/)
- [Participant Information](https://www.nceas.ucsb.edu/projects/12816)

## Repository Explanation

This repository contains the code needed for one sub-project created by the Long Term Ecological Research (LTER) synthesis working group: **"[From poles to tropics](https://www.nceas.ucsb.edu/workinggroups/lter-si-exports): A multi-biome synthesis investigating the controls on river Si exports"**

## Script Explanations

The 'core' scripts in this repository are described below. Any script not described below is exploratory and should be treated as such.

- **`00_data-download.R`** - Downloads the various source data from this working group's Shared Google Drive. Note that only those with access to that Drive will be able to run the script.

- **`01_sizer-workflow.R`** - Runs SiZer workflow to identify changes in slope for WRTDS time series. Works for monthly, seasonal (averaged across sub-season months), and annual input data.
    - Assumes you have either run `00_data-download.R` or manually put the needed input files in a folder named "data"

- **`02_stats-prep.R`** - Combines various data files and calculates covariate columns likely to be useful in the analysis script. Just like `01_sizer-workflow.R`, works for annual/seasonal/monthly input data
    - Assumes you have run `01_sizer-workflow.R`

- **`03a_stats-dynamic-lmer.R`** - Performs desired statistical analysis and correlation checks
    - Assumes you have run `02_stats-prep.R`

- **`03b_explore-graphs.R`** - Makes exploratory (i.e., not necessarily publication-quality) graphs of extracted SiZer information. Works for annual/seasonal/monthly input data
    - Assumes you have run `02_stats-prep.R`

- **`03c_site-map.R`** - Creates a map of the sites with an included permafrost raster for additional context. Also points are colored by mean Si concentration.
    - Assumes you have either run `00_data-download.R` _and_ `02_stats-prep.R`

- **`04_figures.R`** - Makes publication-quality figures from extracted SiZer information. Written for _only annual_ data
    - Assumes you have run `02_stats-prep.R`

## Related Repositories

This working group has several repositories. All are linked and described (briefly) below.

- [lter/**lterwg-silica-data**](https://github.com/lter/lterwg-silica-data) - Primary data wrangling / tidying repository for "master" data files
- [SwampThingPaul/**SiSyn**](https://github.com/SwampThingPaul/SiSyn) - Original repository for this working group. Performs many functions from data wrangling through analysis and figure creation
- [lsethna/**NCEAS_SiSyn_CQ**](https://github.com/lsethna/NCEAS_SiSyn_CQ) - Examples concentration (C) and discharge (Q) relationships for a wide range of solutes
- [lter/**lterwg-silica-spatial**](https://github.com/lter/lterwg-silica-spatial) - Extracts spatial and climatic information from within watershed shapefiles
- [njlyon0/**lter_silica-high-latitude**](https://github.com/njlyon0/lter_silica-high-latitude) - Performs analysis and visualization for the high latitude manuscript
