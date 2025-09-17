## Google Drive Interaction Scripts

These files download the 'raw' inputs needed for the first workflow script and then upload the outputs of each script back to the Drive. Each upload script uploads the output(s) of the 'actual' workflow script with the same zero-padded number prefix (e.g., the outputs of `01_sizer-workflow.R` are uploaded to the Drive by `01x_data-upload`).

All scripts in this workflow require access to a Shared Google Drive owned by the Long Term Ecological Research (LTER) synthesis working group [From Poles to Tropics: A Multi-Biome Synthesis Investigating the Controls on River Si Exports](https://lternet.edu/working-groups/from-poles-to-tropics-a-multi-biome-synthesis-investigating-the-controls-on-river-si-exports/). 

However, 'raw' data is actually from the GlASS ([Global Aggregation of Steam Silica](https://www.usgs.gov/data/global-aggregation-stream-silica-glass-ver-20-july-2025)) dataset so you can download that manually and then should be able to reproduce our workflow in the top level of this repository.
