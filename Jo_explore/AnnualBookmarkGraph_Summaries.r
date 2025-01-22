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

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
              # Data Prep ----
## ----------------------------------------- ##

# Grab the desired data file
full_df <- read.csv(file = file.path("stats_ready_annualfromNick", "stats-ready_annual_Conc_uM_DSi.csv")) 
#files are in this folder - just change the file names to get desired output

full_df<-full_df %>%
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great", 
                                            "NA", "NS")))

#create a short stream name column
full_df$streamshort<-paste0(stringr::str_sub(full_df$LTER,1,3),"_",
                                paste0(stringr::str_sub(full_df$Stream_Name,1,20)))

# Make a data object with only the columns that we'll want
core_df <- full_df %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, chemical:section_duration, 
                F_statistic:line_fit, slope_estimate:slope_std_error,
                dplyr::starts_with("dir_"), streamshort) %>%
  # Drop non-unique rows
  dplyr::distinct()


# Check structure
dplyr::glimpse(core_df)

# Remove McMurdo streams with incomplete chemical information
core_df2<-core_df %>%
  dplyr::filter(!Stream_Name %in% c("Commonwealth Stream at C1", "Crescent Stream at F8", 
                             "Delta Stream at F10", "Harnish Creek at F7",
                             "Onyx River at Lake Vanda Weir", "Onyx River at Lower Wright Weir",
                             "Priscu Stream at B1")) 

# Filter the simplified data object to only significant rivers with a good fit
sig_only <- core_df2 %>%
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


#================================================
#across all years all rivers, how many yrs of inc? dec? 
#for each network, how many yrs of increasing and decreasing?

#if want marginal significant too then do this on the core_df

##just viewing and cut/copying data - not exporting via CSV

#SiZerOuts_Summary_Direction
sig_only %>%
  dplyr::group_by(LTER, dir_sig) %>% #add in stream if want to do it by stream
  dplyr::summarize(ct=n(), 
                   avg_perChange = mean(percent_change),
                   sd_perChange = sd(percent_change)) 

#SiZerOuts_Summary_AvgPercentChange
sig_only %>%
  dplyr::group_by(LTER) %>% #add in stream if want to do it by stream
  dplyr::summarize(ct=n(), 
                   avg_perChange = mean(percent_change),
                   sd_perChange = sd(percent_change)) 

#total number of years
sig_only %>%
  dplyr::group_by(LTER, dir_sig) %>%
  dplyr::summarize(ct=n()) #n results in same thing as length. #be careful if number rows proxy looking for
#can do length(Year) to double check. n function wants nothing in the parenthese

#write.csv(SiZerOuts_Summary_Direction, file="SiZerOuts_Summary_Direction_Yield_Si_N.csv", row.names=FALSE)
#write.csv(SiZerOuts_Summary_AvgPercentChange, file="SiZerOuts_Summary_AvgPercentChange_Yield_Si_N.csv", row.names=FALSE)


