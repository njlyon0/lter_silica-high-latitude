## ------------------------------------------------------- ##
          # Anovas to determine differences between sites and months
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey


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

# Identify the filename
file_name <- "stats-ready_nodriversmonthly_Conc_uM_DSi_bw5.csv"

# Grab the desired data file
full_v0 <- read.csv(file = file.path("tidy_data", file_name))


# Glimpse it
dplyr::glimpse(full_v0)

# Do some processing
full_df <- full_v0 %>% 
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great",
                                            "NA", "NS")))

# Check its structure
dplyr::glimpse(full_df)

# Make a data object with only the columns that we'll want
core_df <- full_df %>%
  # Arrange by LTER, site, and month
  dplyr::arrange(LTER, Stream_Name, Month) %>%
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, stream, Month, 
                chemical:section_duration, 
                F_statistic:line_fit, 
                slope_estimate:slope_std_error,
                dplyr::starts_with("dir_"), Latitude) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(core_df)

# What do we drop with that operation?
supportR::diff_check(old = names(full_df), new = names(core_df))

# Filter the simplified data object to only significant rivers with a good fit
sig_only <- core_df %>%
  # Keep only significant slopes
  dplyr::filter(significance %in% c("sig", "marg")) %>%
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>%
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(sig_only)


## ----------------------------------------- ##
# Nick Lyon's Custom Function(s) ----
## ----------------------------------------- ##

aov_process <- function(aov){
  
  # Get the summary table
  mod_table <- as.data.frame(aov$table)
  
  # Get the terms out as a real column (default is as rownames)
  mod_out <- mod_table %>%
    dplyr::mutate(Term = rownames(.), .before = dplyr::everything()) %>%
    # Rename P value while we're here
    dplyr::rename(P_Value = `Pr(>F)`)
  
  # Drop the rownames
  rownames(mod_out) <- NULL
  
  # Return that last object
  return(mod_out) }


###========================================

# Make an empty list to store all of our extracted information
# Pare down to needed columns and unique rows
sig_simp <- sig_only %>%
  dplyr::select(LTER, stream, Month, section_duration, percent_change) %>%
  dplyr::distinct()
names(sig_simp)

giant_list <- list()
#Z score is effect size (check function help file for units - maybe unitless)
#by having the interaction in here, we're able to see if streams within an LTER are different

for(ltername in unique(sig_simp$LTER)){
  message("processing LTER:", ltername)
  one_lter<- sig_simp %>%
    filter(LTER==ltername)
  
  si_conc_mod1 <- RRPP::lm.rrpp(percent_change ~ stream * as.factor(Month),
                                cov=TRUE, data = one_lter, iter = 999)
  si_conc_aov1 <- anova(si_conc_mod1, effect.type = "F")
  si_conc_table1 <- aov_process(si_conc_aov1) %>%
    mutate(LTER=ltername,.before=everything())
  
  giant_list[[ltername]] <- si_conc_table1}

giant_df<- giant_list %>%
  purrr::list_rbind(x=.)

write.csv(giant_df, file="AOV_Monthly_DSi_Conc.csv", row.names=FALSE)



