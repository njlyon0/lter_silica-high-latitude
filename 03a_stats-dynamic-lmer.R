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
#install.packages("librarian")
librarian::shelf(tidyverse, car, lme4, lmerTest, ggResidpanel)
librarian::shelf(Hmisc, corrplot, MASS, MuMIn)

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
          # Silica Concentration ----
## ----------------------------------------- ##
# Read in ready data for this response variable
conc <- df_v1

names(conc)

Driver1 <- conc %>%
  # Drop information not useful for regression model
  # When doing non-conc data need to change "conc_uM" to appropriate term
  dplyr::select(-relative_Year, -Conc_uM, -season:-Year, -temp_degC, -section:-section_end, -F_statistic:-slope_direction,
                -term_statistic:-term_p_value, -temp_degC, -sd_temp_degC, -precip_mm.per.day, -sd_precip_mm.per.day,
                -npp_kgC.m2.year, -sd_npp_kgC.m2.year, -evapotrans_kg.m2,
                -sd_evapotrans_kg.m2, -snow_max.prop.area, sd_snow_max.prop.area, -snow_num.days, -sd_snow_num.days) %>%
  #drop even more columns
  dplyr::select(-sizer_bandwidth, -sd_response, -section_duration, -slope_std_error,
                -land_evergreen_needleleaf_forest, -land_evergreen_broadleaf_forest, -land_mixed_forest, 
                -land_deciduous_broadleaf_forest, -land_deciduous_needleleaf_forest, -sd_snow_max.prop.area) %>%
  # Drop non-unique rows
  unique() %>%
  # Drop McMurdo b/c don't have driver data for that
  dplyr::filter(LTER != "MCM")

# Check structure
dplyr::glimpse(Driver1)
names(Driver1)


##========================================
#first isolate numeric data for scaling and checking for covariation and skew
##========================================

#isolate numeric data
Driver2 <- Driver1 %>%
  #dplyr::select(-sizer_groups, -percent_change, -slope_estimate, -LTER, -Stream_Name) %>%
  dplyr::select(-contains ("major_")) %>%
  dplyr::select(-contains ("land_")) %>%
  dplyr::select(-contains ("sd_"))

names(Driver2)

#examining correlation among variables for time-varying variables
CorExplore <- Driver1 %>%
  dplyr::select(contains("slope_"), starts_with("P_"), Discharge_cms) %>%
  dplyr::select(-contains(c("DIN", "NO3", "NH4", "NOx", "estimate", "FNConc", "Si.")))
CorExplore %>%   
  cor(use="complete.obs") %>%
  corrplot(method = "number") #highest value 0.46 (change in temp and ET) once exclude num snow days (correlated to snow area) 

#examining correlation among variables for static variables
CorExplore2 <- Driver2 %>%
  dplyr::select(mean_npp_kgC.m2.year, mean_precip_mm.per.day, mean_snow_max.prop.area, mean_temp_degC, mean_P_Conc_uM,
                mean_Discharge_cms)
CorExplore2 %>%   
  cor(use="complete.obs") %>%
  corrplot(method = "number") #mean temp correlated w/ mean npp (r = 0.79) and Q (r = 0.61) so if remove temp, highest is 0.51

#combining final predictors into one dataframe
ModelPrep <- Driver1 %>%
  dplyr::select(sizer_groups, Stream_Name, percent_change, LTER, mean_response,
                contains("slope_"), starts_with("mean_P_"), mean_Discharge_cms, 
                mean_npp_kgC.m2.year, mean_precip_mm.per.day, mean_snow_max.prop.area, mean_temp_degC, mean_P_Conc_uM) %>%
  dplyr::select(-contains(c("DIN", "NO3", "NH4", "NOx", "estimate", "FNConc", "Si."))) %>%
  dplyr::distinct()
  
names(ModelPrep)


#=======================================================
#scaling
#this over-writes columns 6-19 w scaled data
ModelPrep[,c(6:19)]  <- scale(ModelPrep[,c(6:19)], center = T, scale = T) 

#remove NAs
ModelPrep1 <- ModelPrep[complete.cases(ModelPrep),]
names(ModelPrep1)
unique(ModelPrep1$LTER)

#keep LTER to say "the relationship differs among LTERs..."
#does change over time in spatial covariates impact the percent change in DSi?
lm2.0 <-lm(percent_change ~ slope_npp_kgC.m2.year + slope_precip_mm.per.day +
                 slope_snow_max.prop.area + slope_temp_degC + slope_P_Conc_uM + slope_Discharge_cms + LTER +
           slope_npp_kgC.m2.year:LTER + slope_precip_mm.per.day:LTER +
             slope_snow_max.prop.area:LTER + slope_temp_degC:LTER + slope_P_Conc_uM:LTER + slope_Discharge_cms:LTER, ModelPrep1)

#relationship between P and %change doesn't vary by LTER
#if interaction term is significant, don't intererpt those variables by themselves (don't look at first couple of rows in this test)
#so can interpret P and Q across all LTERs but not other variables

summary(lm2.0) #base level (estimate for fixed effect) is value for Finland - add estimate of interaction to get coefficent for each LTER
#p values for interaction - not adjusted higher type 1 error (more likely to be sign b/c asking more questions)
# to handle that do pairwise comparisons
#use pavalue from anova - don't need to be adjusted
# for a paper - f value (or t value for pairwise) and p value. don't report Sum Sq. don't need to report DF. 
anova(lm2.0)
resid_panel(lm2.0)


#does average mean value of spatial covarites impact the percent change in DSi?
#no - model terrible fit (adj r2<0)
#but the average values do HIGHLY explain avg Si behavior: 
lm2.6 <-lm(mean_response ~ mean_npp_kgC.m2.year + mean_precip_mm.per.day +
             mean_snow_max.prop.area + mean_P_Conc_uM + mean_Discharge_cms + LTER +
             mean_npp_kgC.m2.year:LTER + mean_precip_mm.per.day:LTER +
             mean_snow_max.prop.area:LTER + mean_P_Conc_uM:LTER + mean_Discharge_cms:LTER, ModelPrep1)
#adj r2 = 0.86!
summary(lm2.6)
anova(lm2.6)
resid_panel(lm2.6)

##=======================================
#pairwise comparisons
##=======================================
#below not using
library(emmeans)
emm1 <- emmeans(lm2.0, pairwise ~ LTER)
emm2 <- emmeans(lm2.0, specs = pairwise ~ slope_snow_max.prop.area:LTER)
simp <-pairs(emm1, simple = "each")
simp1<-pairs(emm1, by = "LTER")
emm1$emmeans
emm1$contrasts
emm2$contrasts
emms<-emmeans(lm2.0, ~slope_temp_degC:LTER)
emms$contrasts
contrast(emms, interaction = "pairwise")

library(rstatix)
#emmeans_test(data = ModelPrep1, formula = percent_change ~ slope_temp_degC, covariate = LTER, p.adjust.method = "fdr")

#USE THIS BELOW - pvalues differ compared to "summary" prob b/c using diff method
emtrends(lm2.0, pairwise~LTER, var = "slope_temp_degC")
emtrends(lm2.0, pairwise~LTER, var = "slope_snow_max.prop.area")
emtrends(lm2.0, pairwise~LTER, var = "slope_P_Conc_uM")
emtrends(lm2.0, pairwise~LTER, var = "slope_precip_mm.per.day")
emtrends(lm2.0, pairwise~LTER, var = "slope_npp_kgC.m2.year") #anova says significant, but pairwise says not
#in paper say, "ANCOVA shows significant interaction but pairwise does not after performing the tukey correction for multiple comparisons"

#visualize the lm results
#maybe use uncentered data (use real data) for intuition
#bc our stats account for variation for center and scaling
ggplot(ModelPrep1, aes(y = percent_change, x = slope_npp_kgC.m2.year)) +
  geom_point(aes(color = LTER, shape = LTER)) +
  geom_smooth(aes(color = LTER), method = "lm", se = F)

#pairwise for lm predicting avg response (not change)
#USE THIS BELOW - pvalues differ compared to "summary" prob b/c using diff method
emtrends(lm2.6, pairwise~LTER, var = "mean_Discharge_cms") 
emtrends(lm2.6, pairwise~LTER, var = "mean_snow_max.prop.area")
emtrends(lm2.6, pairwise~LTER, var = "mean_P_Conc_uM")
emtrends(lm2.6, pairwise~LTER, var = "mean_precip_mm.per.day")
emtrends(lm2.6, pairwise~LTER, var = "mean_npp_kgC.m2.year")

# Exploratory graph
ggplot(ModelPrep1, aes(x = slope_P_Conc_uM, y = percent_change)) +
  geom_point(pch = 21, size = 3, fill = "blue") 
#add a trendline - "if there is a line, it's a sign relationship"


ggplot(ModelPrep1, aes(x = LTER, y = slope_precip_mm.per.day)) +
  #geom_violin() +
  geom_boxplot() 
#facet_wrap(~ LTER, scales = "free_y",
  


###==============================
# Trying RRPP - old IGNORE
# Fit a model of interest
si_mod1 <- RRPP::lm.rrpp(percent_change ~ LTER +
                                ## Dynamic drivers
                           mean_npp_kgC.m2.year +
                           mean_evapotrans_kg.m2 + mean_snow_max.prop.area + slope_precip_mm.per.day +
                           slope_npp_kgC.m2.year + slope_evapotrans_kg.m2 +
                           slope_snow_max.prop.area + slope_temp_degC,
                              data = ModelPrep1, iter = 999)

# Get ANOVA table for that model
si_aov1 <- anova(si_mod1, effect.type = "F",
                      error = c(
                        "land_total_forest", # 'random' effect for LTER
                        "LTER", # random effect for 'slope_temp_degC'
                        "LTER", # random effect for 'slope_precip_mm.per.day'
                        "LTER", # random effect for 'slope_npp_kgC.m2.year'
                        "LTER", # random effect for 'slope_evapotrans_kg.m2'
                        "LTER", # random effect for 'slope_snow_max.prop.area'
                        "LTER", # random effect for 'slope_snow_num.days'
                        "LTER", # random effect for 'Latitude'
                        "LTER", # random effect for 'elevation_mean_m'
                        "LTER", # random effect for 'land_total_forest'
                        "LTER")) # random effect for 'land_barren_or_sparsely_vegetated'

# Specifying "Residuals" in error argument means NO random effect is used
# Specifying any other fixed effect name means that is used as a random effect *for that term*

# Summarize that output (and check it out)
( si_conc_table1 <- aov_process(si_aov1) )

summary(si_mod1, formula = F)
predicted.values <- predict(si_mod1, confidence = 0.95)
plot(ModelPrep3$percent_change, predicted.values$mean)
a<-(predicted.values$mean)

# Export locally
write.csv(x = si_conc_table1, row.names = F, na = '',
          file = file.path("stats_results", "annual_DSi_conc_slope.csv"))





## ----------------------------------------- ##
# Within-LTER Tests ----
## ----------------------------------------- ##

# Subset to a particular LTER
one_lter <- dplyr::filter(si_conc, LTER == "NIVA")

# Fit model
test_mod <- RRPP::lm.rrpp(percent_change ~ Stream_Name,
                          data = one_lter, iter = 999)

# Assess model
summary(test_mod, formula = F)

# Get pairwise comparisons
test_pw <- RRPP::pairwise(fit = test_mod, groups = one_lter$Stream_Name)
summary(test_pw)

# End ----
