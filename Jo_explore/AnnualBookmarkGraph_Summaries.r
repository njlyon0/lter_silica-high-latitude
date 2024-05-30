## ------------------------------------------------------- ##
          # Annual Data - Exploratory Graphing
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# PURPOSE:
## Calculate summary stats displayed in bookmark graphs

# Pre-Requisites:
## This script assumes you've run one of the "...-workflow.R" scripts
## And have subsequently run the "stats-prep.R" script
## So you have the relevant output in the "tidy_data" folder

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, cowplot)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
              # Data Prep ----
## ----------------------------------------- ##

# Grab the desired data file
full_df <- read.csv(file = file.path("tidy_data", "stats-ready_nodriversannual_Conc_uM_DIN_bw5.csv")) 

full_df<-full_df %>%
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great", 
                                            "NA", "NS")))

# Check structure
dplyr::glimpse(full_df)

#create a short stream name column
full_df$streamshort<-paste0(stringr::str_sub(full_df$LTER,1,3),"_",
                                paste0(stringr::str_sub(full_df$Stream_Name,1,20)))

# Make a data object with only the columns that we'll want
core_df <- full_df %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, stream, chemical:section_duration, 
                F_statistic:line_fit, slope_estimate:slope_std_error,
                dplyr::starts_with("dir_"), streamshort) %>%
  # Drop non-unique rows
  dplyr::distinct()


# Check structure
dplyr::glimpse(core_df)

# Filter the simplified data object to only significant rivers with a good fit
sig_only <- core_df %>%
  # Keep only significant slopes - including marginal
  #dplyr::filter(significance %in% c("sig", "marg")) %>%
  # Keep only significant slopes - excluding marginal
  dplyr::filter(significance %in% c("sig")) %>%
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(sig_only)


#================================================
#across all years all rivers, how many yrs of inc? dec? 
#for each network, how many yrs of increasing and decreasing?
names(core_df)

#if want marginal significant too then do this on the core_df

sig_only %>%
  dplyr::group_by(LTER, dir_sig) %>% #add in stream if want to do it by stream
  dplyr::summarize(ct=n(), 
                   avg_perChange = mean(percent_change),
                   sd_perChange = sd(percent_change)) #not working b/c this column are factors?
  
sig_only %>%
  dplyr::group_by(dir_sig) %>%
  dplyr::summarize(ct=n()) #n results in same thing as length. #be careful if number rows proxy looking for
#can do length(Year) to double check. n function wants nothing in the parenthese



 



