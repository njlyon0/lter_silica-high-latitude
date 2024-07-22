## ------------------------------------------------------- ##
                    # Statistical Testing
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

# Purpose:
## Do statistical testing to test hypotheses
## May include frequentist stats (P values) and model selection
## Focus is on linear models of dynamic predictors

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, car, lme4, lmerTest, ggResidpanel)
librarian::shelf(RRPP, Hmisc, corrplot, MASS)
            
# Clear environment
rm(list = ls())

# Make needed folder(s)
dir.create(path = file.path("stats_results"), showWarnings = F)

# Load custom functions
for(fxn in dir(path = file.path("tools"), pattern = "fxn_")){
  source(file.path("tools", fxn))
}

## And remove loop index object from environment
rm(list = "fxn")

# Identify desired prepared output
prepped_file <- "stats-ready_annual_Conc_uM_DSi.csv"
# prepped_file <- "stats-ready_monthly_Conc_uM_DSi.csv"

# Read in that SiZer output
df_v1 <- read.csv(file = file.path("data", prepped_file))

# Check structure
dplyr::glimpse(df_v1)

## ----------------------------------------- ##
            # Data Wrangling ----
## ----------------------------------------- ##

# Disclaimer:
## Yes, most of this is handled in '02_stats-prep.R'
## However, some tweaks make more sense to do directly before analysis

# Make a data object with one row / sizer group
sizer_grp_df <- df_v1 %>% 
  # Pare down to needed columns
  dplyr::select(sizer_groups:season, dplyr::contains(c("_response", "section")),
                F_statistic:adj_r_squared, percent_change, slope_direction:slope_std_error,
                Latitude, elevation_mean_m, dplyr::starts_with(c("major_", "land_")),
                dplyr::starts_with(c("mean_", "sd_", "slope_"))) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# What is excluded?
supportR::diff_check(old = names(df_v1), new = names(sizer_grp_df))

# Check structure
dplyr::glimpse(sizer_grp_df)

# Make another data object where annual information is retained
year_df <- df_v1 %>% 
  dplyr::select(sizer_groups:mean_response, dplyr::starts_with("section"),
                F_statistic:adj_r_squared, slope_direction:slope_std_error,
                dplyr::starts_with(c("temp_", "precip_", "npp_", "evapotrans_", 
                                     "snow_", "Discharge_", "DSi_", "P_", "NO3_",
                                     "NH4_", "NOx_", "DIN_", "Si.P_", "Si.DIN_"))) %>%
  dplyr::select(-mean_response) %>% 
  dplyr::distinct()

# What is excluded?
supportR::diff_check(old = names(df_v1), new = names(year_df))

# Check structure
dplyr::glimpse(year_df)

## ----------------------------------------- ##
# Correlation Check - Sizer Groups
## ----------------------------------------- ##

# Check structure of relevant data object
dplyr::glimpse(sizer_grp_df)






# Clear environment of unneeded things
rm(list = setdiff(ls(), c("df_v1", "sizer_grp_df", "year_df")))

## ----------------------------------------- ##
# Analysis - SiZer Groups ----
## ----------------------------------------- ##

# Check structure of relevant data object
dplyr::glimpse(sizer_grp_df)

# Q: Is % change (of response over time w/in SiZer group) affected by ____?
sizer_grp_mod1 <- lmerTest::lmer(percent_change ~ 
                                   ## Change in precip over the same time period
                                   slope_precip_mm.per.day + 
                                   ## Change in ET over the same time period
                                   slope_evapotrans_kg.m2 + 
                                   ## Change in NPP over the same time period
                                   slope_npp_kgC.m2.year + 
                                   ## Change in temp over the same time period
                                   slope_temp_degC + 
                                   ## Change in P concentration over the same time period
                                   slope_P_Conc_uM +
                                   ## After accounting for the fact that streams from the same LTER are likely to be more similar to one another
                                   (1|LTER), 
                                 data = sizer_grp_df)

# Check results
summary(sizer_grp_mod1)

# Check VIF
car::vif(mod = sizer_grp_mod1)

# Check model assumptions
ggResidpanel::resid_panel(model = sizer_grp_mod1)

# Clear environment of unneeded things
rm(list = setdiff(ls(), c("df_v1", "sizer_grp_df", "year_df")))

## ----------------------------------------- ##
# Correlation Check - Yearly
## ----------------------------------------- ##

# Check structure of relevant data object
dplyr::glimpse(year_df)






# Clear environment of unneeded things
rm(list = setdiff(ls(), c("df_v1", "sizer_grp_df", "year_df")))

## ----------------------------------------- ##
# Analysis - Yearly ----
## ----------------------------------------- ##

# Check structure of relevant data object
dplyr::glimpse(year_df)

# Q: Does the response differ among LTERs and/or with drainage area?
year_mod1 <- lm(Conc_uM ~ LTER + drainSqKm + LTER:drainSqKm, data = year_df)

# Check results
summary(year_mod1)

# Check VIF & model assumptions
car::vif(mod = year_mod1)
ggResidpanel::resid_panel(model = year_mod1)

# Clear environment of unneeded things
rm(list = setdiff(ls(), c("df_v1", "sizer_grp_df", "year_df")))

# End ----
