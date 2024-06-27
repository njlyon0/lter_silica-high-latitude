## ------------------------------------------------------- ##
                  # Analysis
## ------------------------------------------------------- ##
# 
# Pre-Requisites:
## This script assumes you've run the "stats-prep.R" script
## And have the relevant output in a "tidy_data" folder

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, RRPP)

# Make a folder for outputting results
dir.create(path = file.path("analysis_March24"), showWarnings = F)

# Clear environment
rm(list = ls())

setwd("~/LNO_Si_Synthesis/CryoAnalysis_2023")
Data<-readr::read_csv('Full_Results_WRTDS_monthly.csv')
#Data<-readr::read_csv('Full_Results_WRTDS_annual.csv')
names(Data)[names(Data) == "Stream_Name"] <- "stream"


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


## ----------------------------------------- ##
          # Silica Concentration ----
## ----------------------------------------- ##
# Read in ready data for this response variable
si_conc <- read.csv(file = file.path("tidy_data", "stats-ready_nodriversannual_Conc_uM_DSi_bw5.csv"))

#si_conc <- read.csv(file = file.path("tidy_data", "stats-ready_nodriversmonthly_Conc_uM_DSi_bw5.csv")) 
names(si_conc)

a<-aov(percent_change ~Stream_Name, data=Niva)
summary(a)

#calcuting average percent change across all years for each month for each stream
si_conc2 <- si_conc %>%
  # Drop non useful columns
  dplyr::select(-sizer_groups, -sizer_bandwidth, -season:-sd_response, -section:-section_end) %>%
  dplyr::select(-F_statistic:-significance, -line_fit:-slope_direction, -term_statistic, -term_p_value) %>% 
  dplyr::group_by(LTER, Stream_Name, stream, Month) %>% #add Month here for Monthly Data
  dplyr::summarize(avg_perChange = mean(percent_change, na.rm = T),
                   avg_duration = mean(section_duration, na.rm = T), 
                   avg_slope = mean(slope_estimate, na.rm = T))
  #dplyr::ungroup() %>%
  #dplyr::group_by(LTER, Stream_Name, stream) %>%
  #dplyr::summarize(avg_duration = mean(section_duration, na.rm = T)) %>%
  #dplyr::summarize(avg_estimate_bymonth = mean(slope_estimate, na.rm = T)) %>%

#ASK NICK - HOW DO WE USE HIS FUNCTION AND HOW DO WE GET A PVALUE
#ASK NICK - HOW DO WE DO ANOVAS TO LOOP THROUGH ALL NETWORKS 
#want to see if differences in responses across streams within a network, also across months within a stream (need monthly for that)
Niva<-subset(si_conc2, LTER == "NIVA")
a<-aov(avg_perChange ~ stream, data=Niva)
aovsummary<-summary(a)[[1]][1,5]
pvalue<-aovsummary[[1]]$'Pr(>F)'
 
#calcuting average percent change across all years for each stream
si_conc <- si_conc %>%
  # Drop annual information
  dplyr::select(-sizer_groups, -sizer_bandwidth, -season:-sd_response, -section:-section_end) %>%
  #Drop other non useful data
  dplyr::select(-F_statistic:-significance, -line_fit:-slope_direction, -term_statistic, -term_p_value) %>% 
  #dplyr::select(-dplyr::starts_with("sd_"))%>%
  #dplyr::select(-dplyr::starts_with("sd_"))%>%
  #over-writing the "major rock"column - if LTER is MCM, then make it glacial till - but no elev or temp data for mcm
  #mutate(major_rock = ifelse( LTER == "MCM", yes = "glacial till", no = major_rock))%>%
  #mutate(major_land = ifelse( LTER == "MCM", yes = "barren", no = major_land))%>%
  #or can just drop MCM
  #dplyr::filter(LTER!="MCM")%>%
  # Drop non-unique rows
  dplyr::group_by(LTER, Stream_Name, stream) %>% #add Month here for Monthly Data
  #dplyr::summarize(avg_perChange_bymonth = mean(percent_change, na.rm = T)) %>%
  dplyr::summarize(avg_duration_bymonth = mean(section_duration, na.rm = T)) %>%
  dplyr::summarize(avg_duration_bymonth = mean(slope_estimate, na.rm = T)) %>%
  dplyr::ungroup() %>%
  unique()



#====================================
###ANOVAS to see if there are differences across streams, months, LTERs####

# Fit a model of interest
#difference btwn RRPP vs aov is the RRPP has fewer assumptions
#having plus sign with LTER is same thing as (actually better) as subsetting by LTER


#are there differences in responses among streams of a single LTER?
#make for loop to one LTER

# Make an empty list to store all of our extracted information
giant_list <- list()

#use code below on annual data to see generally if there are differences within a single LTER
for(ltername in unique(si_conc$LTER)){
  message("processing LTER:", ltername)
  one_lter<- si_conc %>%
    filter(LTER==ltername)
  
  si_conc_mod1 <- RRPP::lm.rrpp(percent_change ~ Stream_Name,
                                cov=TRUE, data = one_lter, iter = 999)
  si_conc_aov1 <- anova(si_conc_mod1, effect.type = "F")
  si_conc_table1 <- aov_process(si_conc_aov1) %>%
    mutate(LTER=ltername,.before=everything())
  
  giant_list[[ltername]] <- si_conc_table1}

giant_df<- giant_list %>%
  purrr::list_rbind(x=.)







#old code looking at differences across LTERs by stream and month
#si_conc_mod1 <- RRPP::lm.rrpp(percent_change ~ Stream_Name:Month + LTER,
                              #cov=TRUE, data = si_conc, iter = 999)


str(si_conc_aov1)

# Summarize that output (and check it out)
(  )

#do pairwise function to see where we see differences
RRPP::pairwise(fit=si_conc_mod1, groups= as.factor(Niva$Stream_Name))

# Export locally
write.csv(x = si_conc_aov1, row.names = F, na = '',
          file = file.path("stats_results", "annual_DSi_conc_slope.csv"))



##===============================================
#### Integrating discharge on monthly WRTDS Output####
# Clear environment
rm(list = ls())

#load data
setwd("~/LNO_Si_Synthesis/CryoAnalysis_2023")
data_v0 <- readr::read_csv(file = "Full_Results_WRTDS_monthly.csv")
names(data_v0)
names(data_v0)[names(data_v0) == "Stream_Name"] <- "stream"


# Now subset to sites of interest
data_simp <- data_v0 %>%
  # Keep only cryosphere LTERs
  dplyr::filter(LTER %in% c("MCM", "GRO", "NIVA", "Krycklan",
                            "Finnish Environmental Institute", "Canada", "Swedish Goverment")) %>%
  # But drop problem sites that are otherwise retained
  dplyr::filter(!stream %in% c("Site 69038", "Kymijoki Ahvenkoski 001",
                               "Kymijoki Kokonkoski 014",
                               "BEAVER RIVER ABOVE HIGHWAY 1 IN GLACIER NATIONAL PARK",
                               "KICKING HORSE RIVER AT FIELD IN YOHO NATIONAL PARK", 
                               "SKEENA RIVER AT USK",
                               "KOOTENAY RIVER ABOVE HIGHWAY 93 IN KOOTENAY NATIONAL PARK",
                               "Helgean Hammarsjon", "Ronnean Klippan", "Morrumsan Morrum", "Lyckebyan Lyckeby",
                               "Lagan Laholm", "Nissan Halmstad", "Atran Falkenberg", "Alsteran Getebro", "Eman Emsfors",
                               "Viskan Asbro", "Gota Alv Trollhattan")) %>%
  # Drop McMurdo in the winter
  dplyr::filter(LTER != "MCM" | (LTER == "MCM" & season != "winter")) %>%
  # Calculate number of years
  dplyr::group_by(LTER, stream) %>%
  dplyr::mutate(num_years = length(unique(Year)), .after = Year) %>%
  dplyr::ungroup() %>%
  # Filter to only more than some threshold years
  dplyr::filter(num_years >= 12) %>%
  # Drop that column now that we've used it
  dplyr::select(-num_years) %>%
  # Convert 10^-6 xx to just xx
  dplyr::mutate(Flux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                    yes = (Flux_10_6kg_yr * 10^6),
                                    no = NA),
                FNFlux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (FNFlux_10_6kg_yr * 10^6),
                                      no = NA),
                Flux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (Flux_10_6kmol_yr * 10^6),
                                      no = NA),
                FNFlux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                        yes = (FNFlux_10_6kmol_yr * 10^6),
                                        no = NA),
                Yield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                           yes = (Yield_10_6kmol_yr_km2 * 10^6),
                                           no = NA),
                FNYield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                             yes = (FNYield_10_6kmol_yr_km2 * 10^6),
                                             no = NA)) %>%
  # Tweak chemical names to exclude `:` in ratio
  dplyr::mutate(chemical = dplyr::case_when(
    chemical == "Si:DIN" ~ "Si_DIN",
    chemical == "Si:P" ~ "Si_P",
    TRUE ~ chemical)) %>%
  # Remove all instances where month info is missing
  dplyr::filter(!is.na(Month)) %>%
  # Flip to long format to get all response variables into a single column
  tidyr::pivot_longer(cols = Discharge_cms:FNYield_kmol_yr_km2,
                      names_to = "var",
                      values_to = "value") %>%
  # Group by everything and average response values
  dplyr::group_by(LTER, stream, drainSqKm, Month, Year, chemical, var) %>%
  dplyr::summarize(value = mean(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = var, values_from = value)

str(data_simp)

#for each month for each stream sum all discharge

data_simp %>%
  dplyr::group_by(LTER, stream, Month) %>%
  dplyr::summarize(QSum = sum(Discharge_cms, na.rm = T))




