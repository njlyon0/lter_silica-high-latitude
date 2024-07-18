## ------------------------------------------------------- ##
                  # Statistical Testing - Linear models of dynamic predictors
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

#add stream into data frame to use as random effect
#try multiple random effects first, then use AIC to choose predictors
#summarize change by stream and then run antoher model with static drivers

# PURPOSE:
## Do statistical testing to test hypotheses
## May include frequentist stats (P values) and model selection

# Pre-Requisites:
## This script assumes you've run the "stats-prep.R" script
## And have the relevant output in a "tidy_data" folder

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, RRPP)
              library(Hmisc)
              library(corrplot) #use this to identify correlations
              require(MASS)
              library(car)
              require(lme4)
              

# Make a folder for outputting results
dir.create(path = file.path("stats_results"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
          # Silica Concentration ----
## ----------------------------------------- ##
# Read in ready data for this response variable
si_conc <- read.csv(file = file.path("tidy_data", "stats-ready_annual_Conc_uM_DSi_bw5.csv")) 
names(si_conc)

Driver1 <- si_conc %>%
  # Drop information not useful for regression model
  # When doing non-conc data need to change "conc_uM" to appropriate term
  dplyr::select(-relative_Year, -Conc_uM, -season:-Year, -temp_degC, -section:-section_end, -F_statistic:-slope_direction,
                -term_statistic:-term_p_value, -temp_degC, -sd_temp_degC, -precip_mm.per.day, -sd_precip_mm.per.day,
                -npp_kg.C.m2.year, -sd_npp_kg.C.m2.year, -evapotrans_kg.m2,
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
Driver_numeric <- Driver1 %>%
  dplyr::select(-sizer_groups, -percent_change, -slope_estimate, -LTER, -Stream_Name) %>%
  dplyr::select(-contains ("major_"))
   
names(Driver_numeric)

Driver_numeric_meterol <- Driver_numeric %>%
  #pull out lat and elevation
  dplyr::select(-mean_response, -elevation_mean_m) %>%
  dplyr::select(contains("mean_")) %>%
  #temp correlated with basically everything! esp npp
  dplyr::select(-mean_temp_degC, -mean_snow_num.days) %>% 
  #pairs ()
  cor(use="complete.obs") %>%
  corrplot(method = "number") #highest value 0.53. 

Driver_numeric_meterol2 <- Driver_numeric %>%
  dplyr::select(contains("slope_")) %>%
  #remove slope of num snow days b/c correlated with slope of snow max prop area
  dplyr::select(-slope_snow_num.days) %>%
  #pairs ()
  cor(use="complete.obs") %>%
  corrplot(method = "number") #highest value -0.51

Driver_numeric2 <- Driver_numeric %>%
  dplyr::select(mean_precip_mm.per.day, mean_npp_kg.C.m2.year, mean_evapotrans_kg.m2, 
  mean_snow_max.prop.area, slope_precip_mm.per.day, slope_evapotrans_kg.m2,
  slope_npp_kg.C.m2.year, slope_temp_degC) %>%
  cor(use="complete.obs") %>%
  corrplot(method = "number") #all together can see highest correlation is -0.58

#combining final predictors into one dataframe
ModelPrep <- Driver1 %>%
  dplyr::select(sizer_groups, Stream_Name, percent_change, LTER,  
                mean_precip_mm.per.day, mean_npp_kg.C.m2.year, mean_evapotrans_kg.m2, 
                mean_snow_max.prop.area, contains("slope_")) %>%
  dplyr::select(-slope_snow_num.days, -slope_estimate) 
names(ModelPrep)



#=======================================================
#scaling
#this over-writes columns 5-13 w scaled data
ModelPrep[,c(5:13)]  <- scale(ModelPrep[,c(5:13)], center = T, scale = T) 

#remove NAs
ModelPrep1 <- ModelPrep[complete.cases(ModelPrep),]
names(ModelPrep1)
unique(ModelPrep1$Stream_Name)

#running various mixed models to see impact of various random effects
lmer2 <-lmer(percent_change ~ mean_precip_mm.per.day + mean_npp_kg.C.m2.year + mean_evapotrans_kg.m2 + 
               mean_snow_max.prop.area + slope_precip_mm.per.day + slope_evapotrans_kg.m2 +
               slope_npp_kg.C.m2.year + slope_temp_degC + (1|LTER), ModelPrep1)

lmer3 <-lmer(percent_change ~ mean_precip_mm.per.day + mean_npp_kg.C.m2.year + mean_evapotrans_kg.m2 + 
               mean_snow_max.prop.area + slope_precip_mm.per.day + slope_evapotrans_kg.m2 +
               slope_npp_kg.C.m2.year + slope_temp_degC + (1|LTER/Stream_Name), ModelPrep1)

AIC_model<-AIC(lmer1, lmer2, lmer3) #lowest AIC is best - these are basically equal

summary(lmer2)
vif(lmer_dynamic)
#r2 marginal explains the variance explained by fixed effects
#r2 conditional explains variance explianed by entire model, including fixed and random effects
r.squaredGLMM(lmer1)

#trying regular lm model
lm <-lm(percent_change ~ mean_precip_mm.per.day + mean_npp_kg.C.m2.year + mean_evapotrans_kg.m2 + 
               mean_snow_max.prop.area + slope_precip_mm.per.day + slope_evapotrans_kg.m2 +
               slope_npp_kg.C.m2.year + slope_temp_degC, ModelPrep1)

AIC_model<-stepAIC(object = lm, direction = "both", trace = TRUE)

lm2 <-lm(percent_change ~ mean_evapotrans_kg.m2 + 
          slope_precip_mm.per.day + 
          slope_npp_kg.C.m2.year + slope_temp_degC, ModelPrep1)
summary(lm2) #R2 = 0.22
hist(resid(lm2)) #looks good

# Exploratory graph
ggplot(ModelPrep1, aes(x = slope_npp_kg.C.m2.year, y = percent_change)) +
  geom_point(pch = 21, size = 3, fill = "blue") 
  
#===================================================
#===================================================
#lm model w/ just slopes- USE THIS ONE but need to add P 
lm1<-lm(percent_change ~ slope_precip_mm.per.day +
              slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
              slope_snow_max.prop.area + slope_temp_degC, ModelPrep1)
summary(lm1) #adj r2 = 0.27, p value sig for everything except et
AIC_model<-stepAIC(object = lm1, direction = "both", trace = TRUE)

#post AIC
lm1<-lm(percent_change ~ slope_precip_mm.per.day +
          slope_npp_kg.C.m2.year + 
          slope_snow_max.prop.area + slope_temp_degC, ModelPrep1)

#add LTER as fixed effect
lm2<-lm(percent_change ~ slope_precip_mm.per.day +
          slope_npp_kg.C.m2.year + 
          slope_snow_max.prop.area + slope_temp_degC + LTER, ModelPrep1)
#add LTER as random effect
lmer2<-lmer(percent_change ~ slope_precip_mm.per.day +
          slope_npp_kg.C.m2.year + 
          slope_snow_max.prop.area + slope_temp_degC + (1|LTER), ModelPrep1)
summary(lm1) #adj r2 = 0.27, p value sig
summary(lm2) #adj r2 = 0.27, p value sig
vif(lm1) #looks good
hist(resid(lm1)) #ok
r.squaredGLMM(lmer2)
summary(lmer2)



###==============================
# Trying RRPP - old IGNORE
# Fit a model of interest
si_mod1 <- RRPP::lm.rrpp(percent_change ~ LTER +
                                ## Dynamic drivers
                           mean_npp_kg.C.m2.year +
                           mean_evapotrans_kg.m2 + mean_snow_max.prop.area + slope_precip_mm.per.day +
                           slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
                           slope_snow_max.prop.area + slope_temp_degC,
                              data = ModelPrep1, iter = 999)

# Get ANOVA table for that model
si_aov1 <- anova(si_mod1, effect.type = "F",
                      error = c(
                        "land_total_forest", # 'random' effect for LTER
                        "LTER", # random effect for 'slope_temp_degC'
                        "LTER", # random effect for 'slope_precip_mm.per.day'
                        "LTER", # random effect for 'slope_npp_kg.C.m2.year'
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
