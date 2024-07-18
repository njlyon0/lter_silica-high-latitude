## ------------------------------------------------------- ##
                  # Statistical Testing
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

# Make a folder for outputting results
dir.create(path = file.path("stats_results"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
          # Custom Function(s) ----
## ----------------------------------------- ##

# Neat little tool to summarize key bits of aov table
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

#remove response variables and unique identifyer and examine coorelation among predictors

#elevation highly correlated with tundra and forest so just have forest here
#removed wetland, developed, cropland b/c many outliers
Driver_numeric_land <- Driver_numeric %>%
  #pull out just the land and lat to see if related
  dplyr::select(Latitude, contains("land_")) %>%
  dplyr::select(-land_barren_or_sparsely_vegetated, -land_cropland, -land_urban_and_built_up_land, 
         -land_tundra, -land_wetland) %>%
  pairs ()
#keeping just Latitude, total forest, shrub-grassland
#except below we have to remove Lat and total forest b/c correlated with mean temp

Driver_numeric_meterol <- Driver_numeric %>%
  #pull out lat and elevation
  dplyr::select(-mean_response, -elevation_mean_m) %>%
  #pull out just the land and lat to see if related
  dplyr::select(contains("mean_")) %>%
  #temp correlated with basically everything! just keep temp
  dplyr::select(-mean_temp_degC, -mean_snow_num.days) %>%
  #select(-mean_precip_mm.per.day, -mean_npp_kg.C.m2.year, -mean_snow_num.days) %>%
  pairs ()

Driver_numeric_meterol2 <- Driver_numeric %>%
  #pull out just the land and lat to see if related
  dplyr::select(contains("slope_")) %>%
  #remove slope of num snow days b/c correlated with slope of snow max prop area
  dplyr::select(-slope_snow_num.days) %>%
  pairs ()

names(Driver_numeric)

Driver2 <- Driver_numeric %>%
  dplyr::select(contains("slope_")) %>%
  #remove slope of num snow days b/c correlated with slope of snow max prop area
  dplyr::select(-slope_snow_num.days) %>%
  cor(use="complete.obs") %>%
  corrplot(method = "number")
#pick a threshold and just report it. scale. then use stepwise

ModelPrep <- Driver1 %>%
  dplyr::select(sizer_groups, Stream_Name, percent_change, LTER, drainSqKm, Latitude, land_total_forest, 
                mean_precip_mm.per.day, 
         mean_npp_kg.C.m2.year, mean_evapotrans_kg.m2, mean_snow_max.prop.area, contains("slope_")) 
  #remove slope of num snow days b/c correlated with slope of snow max prop area
  #cor(use="complete.obs") %>%
  #corrplot(method = "number")



#=======================================================
#scaling
#this over-writes columns 4-17 w scaled data
ModelPrep[,c(4:17)]  <- scale(ModelPrep[,c(4:17)], center = T, scale = T) 

ModelPrep2 <- ModelPrep
#remove NAs
ModelPrep3 <- ModelPrep2[complete.cases(ModelPrep2),]
names(ModelPrep2)

require(MASS)
library(car)

#what about divide up drivers by dynamic vs static (over time)?
#really should then look at avg change over entire period per river rather than this way
lm_static <- lm(percent_change~drainSqKm + Latitude + land_total_forest + mean_npp_kg.C.m2.year +
                  mean_evapotrans_kg.m2 + mean_snow_max.prop.area, ModelPrep2)
summary(lm_static) #wow - horrible fit!

#dynamic only 
lm1<-lm(percent_change ~ slope_precip_mm.per.day +
              slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
              slope_snow_max.prop.area + slope_temp_degC, ModelPrep2)
summary(lm1) #adj r2 = 0.27, p value sig
AIC_model<-stepAIC(object = lm1, direction = "both", trace = TRUE)

#post AIC
lm1<-lm(percent_change ~ slope_precip_mm.per.day +
          slope_npp_kg.C.m2.year + 
          slope_snow_max.prop.area + slope_temp_degC, ModelPrep2)
summary(lm1) #adj r2 = 0.27, p value sig


#all 
lm_all<-lm(percent_change ~ drainSqKm + Latitude + land_total_forest + 
             mean_npp_kg.C.m2.year + mean_evapotrans_kg.m2 + mean_snow_max.prop.area + 
             slope_precip_mm.per.day +
          slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
          slope_snow_max.prop.area + slope_temp_degC, ModelPrep3)
summary(lm_all)

AIC_model<-stepAIC(object = lm_all, direction = "both", trace = TRUE)
#post AIC
lm_all2<-lm(percent_change ~ mean_evapotrans_kg.m2 + 
             slope_precip_mm.per.day +
             slope_npp_kg.C.m2.year + 
             slope_snow_max.prop.area + slope_temp_degC, ModelPrep3)
summary(lm_all2)
names(lm_all2)
plot(ModelPrep3$percent_change, lm_all2$fitted.values)


#below is dynamic model for only dynamic drivers  
lm1<-lm(percent_change ~
          slope_precip_mm.per.day +
          slope_npp_kg.C.m2.year + 
          slope_snow_max.prop.area + slope_temp_degC, ModelPrep2)

summary(lm1) #R2 - 0.27
hist(resid(lm1)) #normal looking
shapiro.test(resid(lm1)) #fails - significant

#now add random effect of LTER
require(lme4)
lmer_dynamic1 <-lmer(percent_change~ slope_precip_mm.per.day +
                      slope_npp_kg.C.m2.year + 
                      slope_snow_max.prop.area + slope_temp_degC + (1|stream), ModelPrep3)

lmer_dynamic2 <-lmer(percent_change~ slope_precip_mm.per.day +
                       slope_npp_kg.C.m2.year + 
                       slope_snow_max.prop.area + slope_temp_degC + (1|LTER/stream), ModelPrep3)

lmer_dynamic3 <-lmer(percent_change~ slope_precip_mm.per.day +
             slope_npp_kg.C.m2.year + 
             slope_snow_max.prop.area + slope_temp_degC + (1|LTER), ModelPrep3)
AIC_model<-AIC(model 1, model 2, model 3, direction = "both", trace = TRUE)

summary(lmer_dynamic)
vif(lmer_dynamic)
#r2 marginal explains the variance explained by fixed effects
#r2 conditional explains variance explianed by entire model, including fixed and random effects
r.squaredGLMM(lmer_dynamic3) 


lmer_all2<-lmer(percent_change ~ mean_evapotrans_kg.m2 + 
              slope_precip_mm.per.day +
              slope_npp_kg.C.m2.year + 
              slope_snow_max.prop.area + slope_temp_degC+ (1|LTER), ModelPrep3)
summary(lmer_all2)
vif(lmer_all2)

#r2 marginal explains the variance explained by fixed effects
#r2 conditional explains variance explianed by entire model, including fixed and random effects
r.squaredGLMM(lmer_all2) 
r2_conc<-data.frame(r.squaredGLMM(lmer_all2))
r2_conc$model.name<-c("lmer_all2")

###==============================
# Trying RRPP
# Fit a model of interest
si_mod1 <- RRPP::lm.rrpp(percent_change ~ LTER +
                                ## Dynamic drivers
                           Latitude + land_total_forest + mean_npp_kg.C.m2.year +
                           mean_evapotrans_kg.m2 + mean_snow_max.prop.area + slope_precip_mm.per.day +
                           slope_npp_kg.C.m2.year + slope_evapotrans_kg.m2 +
                           slope_snow_max.prop.area + slope_temp_degC,
                              data = ModelPrep3, iter = 999)

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

# Exploratory graph
ggplot(si_conc, aes(x = abs(Latitude), y = slope_estimate)) +
  geom_point(pch = 21, size = 3, fill = "blue") +
  geom_smooth(method = "lm", se = F, formula = "y ~ x", color = "black") +
  labs(x = "Latitude", y = "Slope of Si Conc. (uM) / Years") +
  supportR::theme_lyon()



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
