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
ggplot2::ggsave(filename = file.path("stats_results", "perc_change_residuals.png"),
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

# Q: Is mean response (of DSi) affected by driver x LTER interactions?
avg_lm <- lm(mean_si_conc ~ scaled_mean_npp_kgC.m2.year + 
                scaled_mean_precip_mm.per.day + scaled_mean_snow_max.prop.area + 
                scaled_mean_temp_degC + scaled_mean_P_Conc_uM + 
                scaled_mean_Discharge_cms + LTER +
                scaled_mean_npp_kgC.m2.year:LTER + 
                scaled_mean_precip_mm.per.day:LTER +
                scaled_mean_snow_max.prop.area:LTER + 
                scaled_mean_temp_degC:LTER + 
                scaled_mean_P_Conc_uM:LTER + 
                scaled_mean_Discharge_cms:LTER,
              data = si_conc_v2)

# Evalulate metrics for model fit / appropriateness
ggResidpanel::resid_panel(model = avg_lm)
ggplot2::ggsave(filename = file.path("stats_results", "avg_response_residuals.png"),
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
          file = file.path("stats_results", "avg_response_DSi_results.csv"))

# Flexibly identify the variables that have significant 'by LTER' interactions
avg_ixn <- avg_results %>% 
  dplyr::filter(stringr::str_detect(string = term, pattern = ":LTER") == TRUE) %>% 
  dplyr::filter(sig == "yes")

# Make empty lists for outputs
avg_pair_list <- list()
avg_cld_list <- list()

# Loop across these variables...
for(avg_variable in gsub(pattern = ":LTER", replacement = "", x = avg_ixn$term)){
  
  # Message
  message("Getting pairwise comparisons for: ", avg_variable)
  
  # Perform pairwise comparisons
  avg_pair_raw <- emmeans::emtrends(object = avg_lm, pairwise ~ LTER, var = avg_variable)
  
  # Wrangle into pure table format
  avg_pair_df <- as.data.frame(avg_pair_raw$contrasts) %>% 
    dplyr::mutate(term = avg_variable, .before = dplyr::everything())
  
  # Also identify compact letter display (CLD)
  avg_pair_cld <- multcompView::multcompLetters(supportR::name_vec(content = avg_pair_df$p.value, 
                                                                    name = avg_pair_df$contrast))
  
  # And get that into a nice table too
  avg_cld_df <- data.frame(term = avg_variable,
                            LTER = names(avg_pair_cld$Letters),
                            cld = avg_pair_cld$Letters)
  
  # Add to list
  avg_pair_list[[avg_variable]] <- avg_pair_df
  avg_cld_list[[avg_variable]] <- avg_cld_df
  
} # Close loop

# Unlist
avg_pairs <- purrr::list_rbind(x = avg_pair_list)
avg_clds <- purrr::list_rbind(x = avg_cld_list)

# Export these too
write.csv(x = avg_pairs, row.names = F, na = '',
          file = file.path("stats_results", "avg_response_DSi_results_pairwise.csv"))
write.csv(x = avg_clds, row.names = F, na = '',
          file = file.path("stats_results", "avg_response_DSi_results_clds.csv"))

# End ----
