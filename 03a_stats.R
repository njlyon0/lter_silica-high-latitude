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
librarian::shelf(tidyverse, ggResidpanel, emmeans, supportR, multcompView, corrplot, broom, car)
              
                

# Make needed folder(s)
dir.create(path = file.path("data", "stats-results"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

# Load custom functions
purrr::walk(.x = dir(path = file.path("tools"), pattern = "fxn_"),
            .f = ~ source(file.path("tools", .x)))

# Read in desired (prepared) output
df_v1 <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))

# Check structure
dplyr::glimpse(df_v1)

## ----------------------------------------- ##
                # Wrangling ----
## ----------------------------------------- ##

# Pre-statistics wrangling
si_conc_v1 <- df_v1 %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical,
                mean_response, percent_change, major_rock, major_land, land_total_forest, land_tundra, 
                land_shrubland_grassland, dplyr::starts_with("slope_"),  dplyr::starts_with("mean_")) %>% 
  dplyr::select(-slope_estimate, -slope_direction, -slope_std_error,
                -dplyr::contains("_FNConc_"),
                -dplyr::contains("_NO3_"), 
                -dplyr::contains("_NH4_"), -dplyr::contains("_NOx_"),
                -dplyr::contains("_Si.DIN_"), -dplyr::contains("_Si.P_")) %>% 
  # Change certain column names to be more informative
  dplyr::rename(mean_si_conc = mean_response,
                perc.change_si_conc = percent_change) %>% 
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream ")) %>%
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

#want to make new major land category of just forest to reduce predictors in model
si_conc_v1<-si_conc_v1 %>%
  dplyr::mutate(major_land2 = case_when(major_land %in% c("evergreen_needleleaf_forest", 
                                                           "mixed_forest", "deciduous_needleleaf_forest") ~ "total_forest",
                                         TRUE ~ major_land))


#move drainage area after major land for scaling and centering to work
si_conc_v1 <- si_conc_v1 %>%
  relocate(drainSqKm, .after=major_land)

# Scale & center the driver variables
scaled_df <- si_conc_v1 %>% 
  dplyr::mutate(dplyr::across(.cols = drainSqKm:mean_Discharge_cms,
                              .fns = ~ as.numeric(scale(x = ., center = T, scale = T)))) %>% 
  # Rename the modified columns to be explicit about what they are
  dplyr::rename_with(.col = drainSqKm:mean_Discharge_cms,
                     .fn = ~ paste0("scaled_", .))

# Integrate the scaled data back into the 'main' data
si_conc_v2 <- si_conc_v1 %>% 
  dplyr::left_join(x = ., y = scaled_df,
                   by = dplyr::join_by(sizer_groups, LTER, Stream_Name, LTER_stream, 
                                       chemical, mean_si_conc, perc.change_si_conc, 
                                       major_rock, major_land2))

# Check structure
dplyr::glimpse(si_conc_v2)

## ----------------------------------------- ##
# Correlation Checks ----
## ----------------------------------------- ##

# Get just 'slope of _' columns in a data object and % forest and tundra
slope_df <- si_conc_v2 %>% 
  dplyr::select(perc.change_si_conc, drainSqKm, dplyr::starts_with("slope_")) %>% 
  dplyr::distinct()

dplyr::glimpse(slope_df)

names(si_conc_v2)

# Generate and export the correlation plot
png(filename = file.path("data", "stats-results", "perc_change_corrplot.png"),
    height = 7, width = 7, units = "in", res = 560)
## Generate plot
slope_corplot <- slope_df %>% 
  stats::cor(x = ., use = "complete.obs") %>% 
  corrplot::corrplot(corr = ., method = "number")
## Exit this saving step
dev.off()

names(si_conc_v2)

# Do the same set of steps for the mean values
## Get mean columns alone
mean_df <- si_conc_v2 %>% 
  dplyr::select(land_total_forest, land_tundra, land_shrubland_grassland, dplyr::starts_with("mean_")) %>% 
  dplyr::distinct()

## Generate and export the correlation plot
png(filename = file.path("data", "stats-results", "avg_response_corrplot.png"),
    height = 7, width = 9, units = "in", res = 560)

mean_corplot <- mean_df %>% 
  stats::cor(x = ., use = "complete.obs") %>% 
  corrplot::corrplot(corr = ., method = "number")

dev.off()

## ----------------------------------------- ##
# % Change Si Response within LTER Across Months ----
## ----------------------------------------- ##

# Read in monthly (prepared) output
month_v1 <- read.csv(file = file.path("data", "stats-ready_monthly", "stats-ready_monthly_Conc_uM_DSi.csv"))

# Check structure
dplyr::glimpse(month_v1)

# Pre-statistics wrangling
month_v2 <- month_v1 %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical, Month,
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
  #drop MCM sites not longer using
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream ")) %>%
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

# Make list for outputs
month_list <- list()

# Loop across LTERs to analyze within each
for(ltername in unique(month_v2$LTER)){
  
  # Processing message
  message("processing LTER: ", ltername)
  
  # Subset the data
  month_sub <- dplyr::filter(.data = month_v2, LTER == ltername)
  
  # Fit linear model
  aov_perc.change_month <- lm(perc.change_si_conc ~ Stream_Name + as.factor(Month),
                              data = month_sub)
  
  # Extract top-level results
  month_results1 <- as.data.frame(stats::anova(object = aov_perc.change_month)) %>%
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
    dplyr::mutate(sig = ifelse(test = p_value < 0.05, 
                               yes = "yes", no = "no")) %>%
    # Add column for LTER
    dplyr::mutate(LTER = ltername, .before = dplyr::everything())
  
  # Generate / export a residual plot (to evaluate model fit)
  ggResidpanel::resid_panel(model = aov_perc.change_month)
  ggplot2::ggsave(filename = file.path("data", "stats-results", paste0("monthly_perc_change_residuals_", ltername, ".png")),
                  height = 5, width = 5, units = "in")
  
  # Add this to the output list
  month_list[[ltername]] <- month_results1
  
} # Close loop

# Unlist the output
month_out <- month_list %>% 
  purrr::list_rbind(x = .)

# Check structure
dplyr::glimpse(month_out)

# Export results
write.csv(x = month_out, row.names = F, na = '',
          file = file.path("data", "stats-results", "monthly_perc_change_DSi_results.csv"))

## ----------------------------------------- ##
# % Change Si Response across LTERs ----
## ----------------------------------------- ##  

# Q: Is percent change (of DSi) affected by driver x LTER interactions?
#interactions with LTER
perc_lm <- lm(perc.change_si_conc ~ scaled_slope_precip_mm.per.day + scaled_drainSqKm + scaled_slope_snow_max.prop.area + 
                scaled_slope_npp_kgC.m2.year + scaled_slope_P_Conc_uM + scaled_slope_DIN_Conc_uM + scaled_slope_temp_degC +
                scaled_slope_evapotrans_kg.m2 + scaled_slope_Discharge_cms + LTER + 
                scaled_slope_precip_mm.per.day:LTER + scaled_slope_snow_max.prop.area:LTER + 
                scaled_slope_temp_degC:LTER + scaled_slope_evapotrans_kg.m2:LTER +
                scaled_slope_P_Conc_uM:LTER + scaled_slope_DIN_Conc_uM:LTER + scaled_slope_Discharge_cms:LTER + 
                scaled_slope_npp_kgC.m2.year:LTER, data = si_conc_v2)

summary(perc_lm) 
AIC(perc_lm) 

#interactions with LTER, with fewer non-significant predictors
perc_lm <- lm(perc.change_si_conc ~ scaled_slope_precip_mm.per.day + scaled_drainSqKm + scaled_slope_snow_max.prop.area +
                scaled_slope_npp_kgC.m2.year + scaled_slope_P_Conc_uM + scaled_slope_DIN_Conc_uM + scaled_slope_temp_degC + 
                scaled_slope_evapotrans_kg.m2 + scaled_slope_Discharge_cms + LTER + 
                scaled_slope_precip_mm.per.day:LTER + scaled_slope_snow_max.prop.area:LTER + 
                scaled_slope_temp_degC:LTER + scaled_slope_evapotrans_kg.m2:LTER +
                scaled_slope_P_Conc_uM:LTER + scaled_slope_Discharge_cms:LTER + 
                scaled_slope_npp_kgC.m2.year:LTER, data = si_conc_v2)

summary(perc_lm) #adjR2 = 0.63 w/o major rock or major land. adj R2 = 0.50 w/ major rock as predictor, adjR2 = 0.50 w/ both major land and rock and w/o rock
AIC(perc_lm) #418 w/o land and rock, 422 w/ major rock as predictor, 424 w/ also major land (and major rock). 422 w/ just major land (and not major rock)

#major rock w/ interactions
perc_lm_rock <- lm(perc.change_si_conc ~ scaled_slope_precip_mm.per.day + scaled_drainSqKm + scaled_slope_snow_max.prop.area + 
                scaled_slope_npp_kgC.m2.year + scaled_slope_P_Conc_uM + scaled_slope_DIN_Conc_uM + scaled_slope_temp_degC +
                scaled_slope_evapotrans_kg.m2 + scaled_slope_Discharge_cms + LTER + major_rock + major_land2 +
                scaled_slope_precip_mm.per.day:major_rock + scaled_slope_snow_max.prop.area:major_rock + 
                scaled_slope_temp_degC:major_rock + scaled_slope_evapotrans_kg.m2:major_rock +
                scaled_slope_P_Conc_uM:major_rock + scaled_slope_DIN_Conc_uM:major_rock + scaled_slope_Discharge_cms:major_rock + 
                scaled_slope_npp_kgC.m2.year:major_rock, data = si_conc_v2)
summary(perc_lm_rock) #adj R2 = 0.39
AIC(perc_lm_rock) #442

#major land w/ interactions
perc_lm_rock <- lm(perc.change_si_conc ~ scaled_slope_precip_mm.per.day + scaled_drainSqKm + scaled_slope_snow_max.prop.area + 
                     scaled_slope_npp_kgC.m2.year + scaled_slope_P_Conc_uM + scaled_slope_DIN_Conc_uM + scaled_slope_temp_degC +
                     scaled_slope_evapotrans_kg.m2 + scaled_slope_Discharge_cms + LTER + major_rock + major_land2 +
                     scaled_slope_precip_mm.per.day:major_land2 + scaled_slope_snow_max.prop.area:major_land2 + 
                     scaled_slope_temp_degC:major_land2 + scaled_slope_evapotrans_kg.m2:major_land2 +
                     scaled_slope_P_Conc_uM:major_land2 + scaled_slope_DIN_Conc_uM:major_land2 + scaled_slope_Discharge_cms:major_land2 + 
                     scaled_slope_npp_kgC.m2.year:major_land2, data = si_conc_v2)
summary(perc_lm_rock) #adj R2 = 0.21
AIC(perc_lm_rock) #466



#export the coefficients, sign of predictors and adj R2 of the model
write.csv(tidy(perc_lm), file = file.path("data", "stats-results", "perc_lm_coef_wlandRocks.csv"))
write.csv(glance(perc_lm), file = file.path("data", "stats-results", "perc_lm_R2wlandRocks.csv"))

# Evalulate metrics for model fit / appropriateness
ggResidpanel::resid_panel(model = perc_lm)
ggplot2::ggsave(filename = file.path("data", "stats-results", "perc_change_residuals_land_rocks_lter.png"),
                height = 5, width = 5, units = "in")

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

#export the fstats
write.csv(perc_results, file = file.path("data", "stats-results", "perc_lm_fstats_wlter_land_rocks.csv"))

#do this above
#lm(Conc_uM ~ stream, data = one_lter)

# Interpretation note:
## Look at interactions first!
## If interaction is sig, effect of driver on DSi % change **depends on LTER**
### Also, interpreting var without interaction doesn't make sense
## If interaction is NS, look at regular variable
## If interaction is NS but variable is sig, driver effects DSi **regardless of LTER**
## If interaction and variable are NS, driver does not (significantly) affect DSi

# Export results - doesn't work so commented it out for now - prob file path issue
#write.csv(x = perc_results, row.names = F, na = '',
          #file = file.path("data", "stats-results", "perc_change_DSi_results.csv"))

# Flexibly identify the variables that have significant 'by LTER' interactions
perc_ixn <- perc_results %>% 
  dplyr::filter(stringr::str_detect(string = term, pattern = ":LTER") == TRUE) %>% 
  dplyr::filter(sig == "yes")

# Make empty list(s) for output(s)
perc_pair_list <- list()

# Loop across these variables...
for(perc_variable in gsub(pattern = ":LTER", replacement = "", x = perc_ixn$term)){
  
  # Message
  message("Getting pairwise comparisons for: ", perc_variable)
  
  # Perform pairwise comparisons
  perc_pair_raw <- emmeans::emtrends(object = perc_lm, pairwise ~ LTER, var = perc_variable)
  
  # Wrangle into pure table format
  perc_pair_df <- as.data.frame(perc_pair_raw$contrasts) %>% 
    dplyr::mutate(term = perc_variable, .before = dplyr::everything())

  # Add to list
  perc_pair_list[[perc_variable]] <- perc_pair_df

} # Close loop

# Unlist
perc_pairs <- purrr::list_rbind(x = perc_pair_list)

# If LTER was significant on its own
if(perc_results[perc_results$term == "LTER", ]$sig == "yes"){
  
  # Fit a simple ANOVA of LTER
  perc_lter_aov <- aov(perc.change_si_conc ~ LTER, data = si_conc_v2)
  
  # Do Tukey HSD pairwise comparisons
  perc_lter_tuk <- TukeyHSD(x = perc_lter_aov, conf.level = 0.95)
  
  # Extract as a table
  perc_lter_pairs <- as.data.frame(perc_lter_tuk$LTER) %>% 
    dplyr::mutate(term = "LTER", .before = dplyr::everything()) %>% 
    tibble::rownames_to_column(var = "contrast") %>% 
    dplyr::rename(estimate = diff,
                  lwr.ci = lwr, upr.ci = upr,
                  p.value = `p adj`)
  
  # Attach each to their respective larger pairwise results object
  perc_pairs <- dplyr::bind_rows(perc_pairs, perc_lter_pairs)
}

# Export these too
write.csv(x = perc_pairs, row.names = F, na = '',
          file = file.path("data", "stats-results", "perc_change_DSi_results_pairwise.csv"))

## ----------------------------------------- ##
              # Mean Si Response ----
## ----------------------------------------- ##

# Q: Is mean response (of DSi) affected by driver x LTER interactions?
avg_lm <- lm(mean_si_conc ~ scaled_land_total_forest + scaled_mean_temp_degC +
                scaled_mean_P_Conc_uM + scaled_mean_evapotrans_kg.m2 +
                scaled_mean_Discharge_cms + LTER + major_rock + scaled_land_total_forest:LTER +
               scaled_mean_evapotrans_kg.m2:LTER +
               scaled_mean_temp_degC:LTER +
               scaled_mean_snow_num.days:LTER + 
               scaled_mean_npp_kgC.m2.year:LTER + 
                scaled_mean_P_Conc_uM:LTER + 
                scaled_mean_Discharge_cms:LTER + 
               scaled_mean_temp_degC:major_rock +
               scaled_slope_snow_num.days:major_rock + 
               scaled_mean_P_Conc_uM:major_rock,
              data = si_conc_v2)
summary(avg_lm)
vif(avg_lm)

#export the coefficients, sign of predictors and adj R2 of the model
write.csv(tidy(avg_lm), file = file.path("data", "stats-results", "avg_lm_coef.csv"))
write.csv(glance(avg_lm), file = file.path("data", "stats-results", "avg_lm_R2.csv"))

# Evalulate metrics for model fit / appropriateness
ggResidpanel::resid_panel(model = avg_lm)
ggplot2::ggsave(filename = file.path("data", "stats-results", "avg_response_residuals.png"),
                height = 5, width = 5, units = "in")

# Extract top-level results
avg_results <- as.data.frame(stats::anova(object = avg_lm)) %>%
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
dplyr::glimpse(avg_results)

# Interpretation note:
## Look at interactions first!
## If interaction is sig, effect of driver on DSi % change **depends on LTER**
### Also, interpreting var without interaction doesn't make sense
## If interaction is NS, look at regular variable
## If interaction is NS but variable is sig, driver effects DSi **regardless of LTER**
## If interaction and variable are NS, driver does not (significantly) affect DSi

# Export results
write.csv(x = avg_results, row.names = F, na = '',
          file = file.path("data", "stats-results", "avg_response_lm_fstats_toplevel.csv"))

# Flexibly identify the variables that have significant 'by LTER' interactions
avg_ixn <- avg_results %>% 
  dplyr::filter(sig == "yes") %>% 
  dplyr::filter(stringr::str_detect(string = term, pattern = ":LTER") == TRUE)

# Make empty lists for outputs
avg_pair_list <- list()

# Loop across these variables...
for(avg_variable in gsub(pattern = ":LTER", replacement = "", x = avg_ixn$term)){
  
  # Message
  message("Getting pairwise comparisons for: ", avg_variable)
  
  # Perform pairwise comparisons
  avg_pair_raw <- emmeans::emtrends(object = avg_lm, pairwise ~ LTER, var = avg_variable)
  
  # Wrangle into pure table format
  avg_pair_df <- as.data.frame(avg_pair_raw$contrasts) %>% 
    dplyr::mutate(term = avg_variable, .before = dplyr::everything())
  
  # Add to list
  avg_pair_list[[avg_variable]] <- avg_pair_df

} # Close loop

# Unlist
avg_pairs <- purrr::list_rbind(x = avg_pair_list)

# If LTER was significant on its own
if(avg_results[avg_results$term == "LTER", ]$sig == "yes"){
  
  # Fit a simple ANOVA of LTER
  avg_lter_aov <- aov(mean_si_conc ~ LTER, data = si_conc_v2)
  
  # Do Tukey HSD pairwise comparisons
  avg_lter_tuk <- TukeyHSD(x = avg_lter_aov, conf.level = 0.95)
  
  # Extract as a table
  avg_lter_pairs <- as.data.frame(avg_lter_tuk$LTER) %>% 
    dplyr::mutate(term = "LTER", .before = dplyr::everything()) %>% 
    tibble::rownames_to_column(var = "contrast") %>% 
    dplyr::rename(estimate = diff,
                  lwr.ci = lwr, upr.ci = upr,
                  p.value = `p adj`)
  
  # Attach each to their respective larger pairwise results object
  avg_pairs <- dplyr::bind_rows(avg_pairs, avg_lter_pairs)
}

# Export these too
write.csv(x = avg_pairs, row.names = F, na = '',
          file = file.path("data", "stats-results", "avg_response_DSi_results_pairwise.csv"))

# End ----
