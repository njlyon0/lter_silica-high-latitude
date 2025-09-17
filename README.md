# Decadal Shifts Towards Higher Riverine Silicon Relative to Nitrogen and Phosphorus Across High Latitudes

[![Code DOI](https://img.shields.io/badge/Code-10.5281/zenodo.17144356-orange.svg)](https://doi.org/10.5281/zenodo.17144356) [![Data DOI](https://img.shields.io/badge/Data-10.5066/P138M8AR-blue.svg)](https://www.sciencebase.gov/catalog/item/66eaf6a0d34e0606a9dbaa19)

Joanna C. Carey, [Nicholas J Lyon](https://njlyon0.github.io/), [Keira Johnson](https://orcid.org/0000-0003-0671-3901), Pamela L. Sullivan, [Sidney A. Bush](https://orcid.org/0000-0002-8359-7927), Lienne Sethna, Ruth Heindel, Hjalmar Laudon, Diane McKnight, Pirkko Kortelainen, Amanda Poste, Antti Raike, Kathi Jo Jankowski

## Script Explanations

All scripts listed below assume that you have run the preceding number script. Scripts with both numbers and letters all require that you run the preceding _numbered_ script but do not have requirements of the other letters that use the same number (i.e., `03a` and `03b` both require that you have run `02` but don't require one another).

- **`01_sizer-workflow.R`** - Runs SiZer workflow to identify changes in slope for WRTDS time series. Works for monthly, seasonal (averaged across sub-season months), and annual input data.
- **`02_stats-prep.R`** - Combines various data files and calculates covariate columns likely to be useful in the analysis script. Just like `01_sizer-workflow.R`, works for annual/seasonal/monthly input data
- **`03a_stats.R`** - Performs desired statistical analysis
- **`03b_explore-graphs.R`** - Makes exploratory (i.e., not necessarily publication-quality) graphs of extracted SiZer information. Works for annual/seasonal/monthly input data
- **`03c_site-map.R`** - Creates a map of the sites with an included permafrost raster for additional context. Also points are colored by mean Si concentration.
- **`03e_figures.R`** - Makes publication-quality figures from extracted SiZer information
- **`04_tables.R`** - Generates publication-quality summary tables

## Related Repositories

This working group has several other repositories. All are linked and described (briefly) below.

- [lter/**lterwg-silica-data**](https://github.com/lter/lterwg-silica-data) - Primary data wrangling / tidying repository for "master" data files
- [SwampThingPaul/**SiSyn**](https://github.com/SwampThingPaul/SiSyn) - Original repository for this working group. Performs many functions from data wrangling through analysis and figure creation
- [lsethna/**NCEAS_SiSyn_CQ**](https://github.com/lsethna/NCEAS_SiSyn_CQ) - Examples concentration (C) and discharge (Q) relationships for a wide range of solutes
- [lter/**lterwg-silica-spatial**](https://github.com/lter/lterwg-silica-spatial) - Extracts spatial and climatic information from within watershed shapefiles
- [njlyon0/**lter_silica-high-latitude**](https://github.com/njlyon0/lter_silica-high-latitude) - Performs analysis and visualization for the high latitude manuscript
