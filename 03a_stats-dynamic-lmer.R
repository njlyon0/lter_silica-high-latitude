## ------------------------------------------------------- ##
                # Hypothesis Testing
## ------------------------------------------------------- ##
# Written by: Joanna Carey & Nick J Lyon

# Purpose:
## Do statistical testing to test hypotheses
## Focus is on linear models of dynamic predictors

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

# Load libraries
#install.packages("librarian")
librarian::shelf(tidyverse, car, lme4, lmerTest, ggResidpanel, Hmisc, emmeans, corrplot, MASS, MuMIn)

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

# Read in desired (prepared) output
df_v1 <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DSi.csv"))

# Check structure
dplyr::glimpse(df_v1)

## ----------------------------------------- ##
# Wrangling ----
## ----------------------------------------- ##

# Pre-statistics wrangling
si_conc_v1 <- df_v1 %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical,
                mean_response, percent_change,
                dplyr::starts_with("slope_"),  dplyr::starts_with("mean_")) %>% 
  dplyr::select(-slope_estimate, -slope_direction, -slope_std_error,
                -dplyr::contains("_FNConc_"),
                -dplyr::contains("_NO3_"), -dplyr::contains("_DIN_"),
                -dplyr::contains("_NH4_"), -dplyr::contains("_NOx_"),
                -dplyr::contains("_Si.DIN_"), -dplyr::contains("_Si.P_")) %>% 
  # Change certain column names to be more informative
  dplyr::rename(mean_si_conc = mean_response,
                perc.change_si_conc = percent_change) %>% 
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

# Scale & center the driver variables
scaled_df <- si_conc_v1 %>% 
  dplyr::mutate(dplyr::across(.cols = slope_P_Conc_uM:mean_Discharge_cms,
                              .fns = ~ as.numeric(scale(x = ., center = T, scale = T)))) %>% 
  # Rename the modified columns to be explicit about what they are
  dplyr::rename_with(.col = slope_P_Conc_uM:mean_Discharge_cms,
                     .fn = ~ paste0("scaled_", .))

# Integrate the scaled data back into the 'main' data
si_conc_v2 <- si_conc_v1 %>% 
  dplyr::left_join(x = ., y = scaled_df,
                   by = dplyr::join_by(sizer_groups, LTER, Stream_Name, LTER_stream, 
                                       drainSqKm, chemical, mean_si_conc, perc.change_si_conc))

# Check structure
dplyr::glimpse(si_conc_v2)

## ----------------------------------------- ##
        # % Change Si Response ----
## ----------------------------------------- ##

# Q: Is percent change (of DSi) affected by driver x LTER interactions?
perc_lm <- lm(perc.change_si_conc ~ scaled_slope_npp_kgC.m2.year + 
                scaled_slope_precip_mm.per.day + scaled_slope_snow_max.prop.area + 
                scaled_slope_temp_degC + scaled_slope_P_Conc_uM + 
                scaled_slope_Discharge_cms + LTER +
                scaled_slope_npp_kgC.m2.year:LTER + 
                scaled_slope_precip_mm.per.day:LTER +
                scaled_slope_snow_max.prop.area:LTER + 
                scaled_slope_temp_degC:LTER + 
                scaled_slope_P_Conc_uM:LTER + 
                scaled_slope_Discharge_cms:LTER,
              data = si_conc_v2)

# Evalulate metrics for model fit / appropriateness
ggResidpanel::resid_panel(model = perc_lm)

# Extract top-level results
perc_results <- as.data.frame(stats::anova(object = perc_lm)) %>%
  # Get terms into column
  tibble::rownames_to_column(.data = ., var = "term") %>% 
  # Drop sum/mean squares columns
  dplyr::select(-`Sum Sq`, -`Mean Sq`) %>% 
  # Rename other columns
  dplyr::rename(deg_free = Df,
                f_stat = `F value`,
                p_value = `Pr(>F)`) %>% 
  # Remove residuals
  dplyr::filter(term != "Residuals") %>% 
  # Identify significance
  dplyr::mutate(sig = ifelse(test = p_value < 0.05, yes = "yes", no = "no"))

# Check structure
dplyr::glimpse(perc_results)

# Interpretation note:
## Look at interactions first!
## If interaction is sig, effect of driver on DSi % change **depends on LTER**
### Also, interpreting var without interaction doesn't make sense
## If interaction is NS, look at regular variable
## If interaction is NS but variable is sig, driver effects DSi **regardless of LTER**
## If interaction and variable are NS, driver does not (significantly) affect DSi

# Export results
write.csv(x = perc_results, row.names = F, na = '',
          file = file.path("stats_results", "perc_change_DSi_results.csv"))

# Flexibly identify the variables that have significant 'by LTER' interactions
perc_ixn <- perc_results %>% 
  dplyr::filter(stringr::str_detect(string = term, pattern = ":LTER") == TRUE) %>% 
  dplyr::filter(sig == "yes")

# Make empty lists for outputs
perc_pair_list <- list()
perc_cld_list <- list()

# Loop across these variables...
for(perc_variable in gsub(pattern = ":LTER", replacement = "", x = perc_ixn$term)){
  
  # Message
  message("Getting pairwise comparisons for: ", perc_variable)
  
  # Perform pairwise comparisons
  perc_pair_raw <- emmeans::emtrends(object = perc_lm, pairwise ~ LTER, var = perc_variable)
  
  # Wrangle into pure table format
  perc_pair_df <- as.data.frame(perc_pair_raw$contrasts) %>% 
    dplyr::mutate(term = perc_variable, .before = dplyr::everything())
  
  # Also identify compact letter display (CLD)
  perc_pair_cld <- multcompView::multcompLetters(supportR::name_vec(content = perc_pair_df$p.value, 
                                                                    name = perc_pair_df$contrast))
  
  # And get that into a nice table too
  perc_cld_df <- data.frame(term = perc_variable,
                            LTER = names(perc_pair_cld$Letters),
                            cld = perc_pair_cld$Letters)
  
  # Add to list
  perc_pair_list[[perc_variable]] <- perc_pair_df
  perc_cld_list[[perc_variable]] <- perc_cld_df
  
} # Close loop

# Unlist
perc_pairs <- purrr::list_rbind(x = perc_pair_list)
perc_clds <- purrr::list_rbind(x = perc_cld_list)

# Export these too
write.csv(x = perc_pairs, row.names = F, na = '',
          file = file.path("stats_results", "perc_change_DSi_results_pairwise.csv"))
write.csv(x = perc_clds, row.names = F, na = '',
          file = file.path("stats_results", "perc_change_DSi_results_clds.csv"))

## ----------------------------------------- ##
# Mean Si Response ----
## ----------------------------------------- ##
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



# Basement ----

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


##======================================================
## two good linear models below - one for percent change and one for mean response
##======================================================

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
  

# End ----
