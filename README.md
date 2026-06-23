# Decadal Shifts Towards Higher Riverine Silicon Relative to Nitrogen and Phosphorus Across High Latitudes

[![Paper DOI](https://img.shields.io/badge/Paper-TBD/TBD-green.svg)](https://github.com/njlyon0/lter_silica-high-latitude) [![Code DOI](https://img.shields.io/badge/Code-10.5281/zenodo.17144356-orange.svg)](https://doi.org/10.5281/zenodo.17144356) [![Data DOI](https://img.shields.io/badge/Data-10.5066/P138M8AR-blue.svg)](https://www.sciencebase.gov/catalog/item/66eaf6a0d34e0606a9dbaa19)

Joanna C. Carey, [Nicholas J Lyon](https://njlyon0.github.io/), [Keira Johnson](https://orcid.org/0000-0003-0671-3901), Pamela L. Sullivan, [Sidney A. Bush](https://orcid.org/0000-0002-8359-7927), Lienne Sethna, Ruth Heindel, Hjalmar Laudon, Diane McKnight, Pirkko Kortelainen, Amanda Poste, Antti Raike, Kathi Jo Jankowski

## Script Explanations

All scripts listed below assume that you have run the preceding number script. Scripts with both numbers and letters all require that you run the preceding _numbered_ script but do not have requirements of the other letters that use the same number (i.e., `03a` and `03b` both require that you have run `02` but don't require one another).

- **`01_sizer-workflow.R`** - Runs SiZer workflow to identify changes in slope for WRTDS time series. Works for monthly, seasonal (averaged across sub-season months), and annual input data.
- **`02_stats-prep.R`** - Combines various data files and calculates covariate columns likely to be useful in the analysis script. Just like `01_sizer-workflow.R`, works for annual/seasonal/monthly input data
- **`03a_stats.R`** - Performs desired statistical analysis
- **`03b_explore-graphs.R`** - Makes exploratory (i.e., not necessarily publication-quality) graphs of extracted SiZer information. Works for annual/seasonal/monthly input data
- **`03c_site-map.R`** - Creates a map of the sites with an included permafrost raster for additional context. Also points are colored by mean Si concentration
- **`03d_land-rock-graphs.R`** - Makes simple graphs of element concentration against lithology/land cover categories
- **`03e_greenup-day.R`** - Analyzes and makes simple graph of greenup day data
- **`03f_figures-actual.R`** - Makes publication-quality figures from extracted SiZer information _included in the main text of the publication_
- **`03g_figures-supplement.R`** - Makes publication-quality figures from extracted SiZer information _included in the supplemental materials of the publication_
- **`03h_figures-bonus.R`** - Makes publication-quality figures from extracted SiZer information _not included in either the main text or the supplemental materials of the publication_
- **`04_tables.R`** - Generates publication-quality summary tables

## Related Work

For more information on work from this group, please visit [our GitHub Organization](https://github.com/global-river-chem).
