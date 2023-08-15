
# Cryosphere Silica Exports

- Primary Investigators: Joanna Carey & Kathi Jo Jankowski
- [Project
  Summary](https://lternet.edu/working-groups/river-si-exports/)
- [Participant Information](https://www.nceas.ucsb.edu/projects/12816)

## Repository Explanation

This repository contains the code needed for one sub-project created by
the Long Term Ecological Research (LTER) synthesis working group:
**“[From poles to
tropics](https://www.nceas.ucsb.edu/workinggroups/lter-si-exports): A
multi-biome synthesis investigating the controls on river Si exports”**

## Script Explanations

Scripts in this repository are described below:

- **`annual-workflow.R`** - Runs SiZer workflow to identify changes in
  slope for *annual* WRTDS data

- **`seasonal-workflow.R`** - Runs SiZer workflow to identify changes in
  slope for *seasonal* WRTDS data. Note that “seasonal” data are derived
  by averaging across sub-season temporal columns in the monthly data.

## Repository Content

See below for a “tree” of this repository’s content:

``` r
supportR::github_tree(repo = "https://github.com/njlyon0/lter_silica-cryosphere", exclude = "_deprecated")
```

    ##                  levelName
    ## 1 .                       
    ## 2  ¦--.gitignore          
    ## 3  ¦--README.Rmd          
    ## 4  ¦--README.md           
    ## 5  ¦--_deprecated         
    ## 6  ¦   °--4 excluded items
    ## 7  ¦--annual-workflow.R   
    ## 8  ¦--seasonal-workflow.R 
    ## 9  °--sizer-report.Rmd

## Related Repositories

This working group has several repositories. All are linked and
described (briefly) below.

- [lter/**lterwg-silica-data**](https://github.com/lter/lterwg-silica-data) -
  Primary data wrangling / tidying repository for “master” data files
- [SwampThingPaul/**SiSyn**](https://github.com/SwampThingPaul/SiSyn) -
  Original repository for this working group. Performs many functions
  from data wrangling through analysis and figure creation
- [lsethna/**NCEAS_SiSyn_CQ**](https://github.com/lsethna/NCEAS_SiSyn_CQ) -
  Examples concentration (C) and discharge (Q) relationships for a wide
  range of solutes
- [lter/**lterwg-silica-spatial**](https://github.com/lter/lterwg-silica-spatial) -
  Extracts spatial and climatic information from within watershed
  shapefiles
- [njlyon0/**lter_silica-cryosphere**](https://github.com/njlyon0/lter_silica-cryosphere) -
  Performs analysis and visualization for the cryosphere manuscript
