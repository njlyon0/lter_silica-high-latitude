# Cryosphere Silica Exports

- Primary Investigators: Joanna Carey & Kathi Jo Jankowski
- [Project Summary](https://lternet.edu/working-groups/river-si-exports/)
- [Participant Information](https://www.nceas.ucsb.edu/projects/12816)

## Repository Explanation

This repository contains the code needed for one sub-project created by the Long Term Ecological Research (LTER) synthesis working group: **"[From poles to tropics](https://www.nceas.ucsb.edu/workinggroups/lter-si-exports): A multi-biome synthesis investigating the controls on river Si exports"**

## Script Explanations

Scripts in this repository are described below:

- **`...-workflow.R`** - Runs SiZer workflow to identify changers in slope for WRTDS time series. Temporal granularity of source data is specified in file prefix ("seasonal" or "annual"). Note that "seasonal" data are derived by averaging across sub-season temporal columns in the monthly data.

- **`stats-prep.R`** - Accepts outputs of _either_ `...-workflow.R` script and (1) attaches basin characteristics / climatic drivers extracted elsewhere and (2) does some summarizing / wrangling needed before stats / visualization.

- **`...-explore-graphs.R`** - Makes exploratory (i.e., not necessarily publication-quality) graphs of extracted SiZer information. Temporal granularity of source data is specified in file prefix ("seasonal" or "annual"). Note that these scripts expect that _both_ the corresponding `...-workflow.R` script and the `stats-prep.R` script have been run and their outputs are in the correct (i.e., default) folders.

- **`ordinations.R`** - Creates ordinations (e.g, PCoA, etc.) of basin characteristics and slope estimates of SiZer-identified time series chunks. Contains sections for both annual and seasonal data so there are not twin scripts for this component.

## Related Repositories

This working group has several repositories. All are linked and described (briefly) below.

- [lter/**lterwg-silica-data**](https://github.com/lter/lterwg-silica-data) - Primary data wrangling / tidying repository for "master" data files
- [SwampThingPaul/**SiSyn**](https://github.com/SwampThingPaul/SiSyn) - Original repository for this working group. Performs many functions from data wrangling through analysis and figure creation
- [lsethna/**NCEAS_SiSyn_CQ**](https://github.com/lsethna/NCEAS_SiSyn_CQ) - Examples concentration (C) and discharge (Q) relationships for a wide range of solutes
- [lter/**lterwg-silica-spatial**](https://github.com/lter/lterwg-silica-spatial) - Extracts spatial and climatic information from within watershed shapefiles
- [njlyon0/**lter_silica-cryosphere**](https://github.com/njlyon0/lter_silica-cryosphere) - Performs analysis and visualization for the cryosphere manuscript
