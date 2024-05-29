## ------------------------------------------------------- ##
          # Annual Data - combining tidy files to plot all chemicals together by lter
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

#NOTE - MOST OF THIS SCRIPT IS WRONG - GOT STUCK W/ THE PIVOT LONGER

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
              # Si Data Prep ----
## ----------------------------------------- ##

# Identify the filename
file_name1 <- "stats-ready_nodriversannual_Conc_uM_DSi_bw5.csv"

# Grab the desired data file
Si_full_v0 <- read.csv(file = file.path("tidy_data", file_name1))


# Glimpse it
dplyr::glimpse(Si_full_v0)

# Do some processing
Si_full_df <- Si_full_v0 %>% 
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great",
                                            "NA", "NS")))

# Check its structure
dplyr::glimpse(Si_full_df)

# Make a data object with only the columns that we'll want
Si_core_df <- Si_full_df %>%
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
dplyr::glimpse(Si_core_df)

# What do we drop with that operation?
supportR::diff_check(old = names(Si_full_df), new = names(Si_core_df))

# Filter the simplified data object to only significant rivers with a good fit
Si_sig_only <- Si_core_df %>%
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

# Check it out
dplyr::glimpse(Si_sig_only)

## ----------------------------------------- ##
# N Data Prep ----
## ----------------------------------------- ##

# Identify the filename
file_name2 <- "stats-ready_nodriversannual_Conc_uM_DIN_bw5.csv"

# Grab the desired data file
N_full_v0 <- read.csv(file = file.path("tidy_data", file_name2))


# Glimpse it
dplyr::glimpse(N_full_v0)

# Do some processing
N_full_df <- N_full_v0 %>% 
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great",
                                            "NA", "NS")))

# Check its structure
dplyr::glimpse(N_full_df)

# Make a data object with only the columns that we'll want
N_core_df <- N_full_df %>%
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
dplyr::glimpse(N_core_df)

# What do we drop with that operation?
supportR::diff_check(old = names(N_full_df), new = names(N_core_df))

# Filter the simplified data object to only significant rivers with a good fit
N_sig_only <- N_core_df %>%
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

# Check it out
dplyr::glimpse(N_sig_only)


## ----------------------------------------- ##
# Merging files ----
## ----------------------------------------- ##

names(Si_sig_only)
names(N_sig_only)

Si_N_sig_only <- full_join(N_sig_only, Si_sig_only, 
                           join_by(sizer_groups, LTER, Stream_Name, stream, Month, Year, Latitude))

#for this to work, need to pivot longer so that "chemical" includes both Si and N for plotting

## ----------------------------------------- ##
              # Plotting Prep ----
## ----------------------------------------- ##

# Grab useful information for informative file names for these graphs
chem <- unique(Si_full_df$chemical)
resp <- gsub(pattern = "_mgL|_uM|_10_6kg_yr|_10_6kmol_yr|_kmol_yr_km2|_kmol_yr|_kg_yr", 
             replacement = "", x = names(Si_full_df)[11])
## Note response identification is dependent upon column order!
names(Si_full_df)

# Assemble this into a file prefix
(file_prefix <- paste0("_", chem, "_", resp, "_"))

# Pick a missing and non significant color
na_col <- "#e5e5e5"
nonsig_col <- "#6c757d"

# Define color palettes
p_palt <- c("NA" = na_col, "sig" = "#132a13",  "marg" = "#006400",  "NS" = nonsig_col)
r2_palt <- c("NA" = na_col,  "bad" = "#b5e48c",  "fine" = "#76c893", 
             "good" = "#1a759f",  "great" = "#184e77")
dir_p_palt <- c("NA" = na_col, "NS" = nonsig_col,
                "pos-sig" = "#ff5400", "pos-marg" = "#ff9e00", 
                "neg-sig" = "#03045e", "neg-marg" = "#00b4d8")
dir_fit_palt <- c("NA" = na_col, "NS" = nonsig_col,
                  "pos-bad" = "#ffe863", "pos-fine" = "#ffe150", 
                  "pos-good" = "#facb2e", "pos-great" = "#f5bd1f",
                  "neg-bad" = "#e4afff", "neg-fine" = "#c86bfa", 
                  "neg-good" = "#722e9a", "neg-great" = "#47297b")





## ----------------------------------------- ##
        # Percent Change Boxplots ----
## ----------------------------------------- ##

# Pare down the data to only needed information
names(Si_sig_only) #need to change this next batch of code from "conc" to "yield" depending on file type

#Si_avgValue<- Si_sig_only %>%
  group_by(LTER)%>%
  #change parameter below to match what's in data - Q, Conc, Yield
  summarize(avg_Value = mean(Conc_uM, na.rm = T)) 

#Si_sig_only <- Si_sig_only %>%
  left_join(Si_avgValue, by = "LTER")
              
#Si_sig_simp <- Si_sig_only %>%
  dplyr::select(LTER, stream, Month, avg_Value, percent_change, Latitude) %>%
  dplyr::distinct()
  
names(Si_N_sig_only)

# Make graph
ggplot(Si_N_sig_only, aes(x = LTER, y = percent_change, fill = avg_Value)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  scale_fill_gradient(low = "red", high = "blue") +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "", y = "Percent Change (%)",
       title = paste("Significant changes in", chem, resp)) +
  theme_classic()+
  theme(legend.position = "bottom",
        strip.background = element_blank())


# Export it
ggsave(filename = file.path("graphs", paste0("sig-only", file_prefix, "perc-change-boxplot_allelements.png")),
       height = 7, width = 11, units = "in")



## ----------------------------------------- ##
# Slope Boxplots - haven't changed this yet to reflect what i want to do ---
## ----------------------------------------- ##

# Pare down the data to only needed information
sig_simp <- sig_only %>%
  dplyr::select(LTER, stream, Month, slope_estimate) %>%
  dplyr::distinct()

# Make graph
ggplot(sig_simp, aes(x = as.factor(Month), y = slope_estimate, fill = LTER)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "Month", y = "Slope",
       title = paste("Significant monthly changes in", chem, resp)) +
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0("monthly_sig-only", file_prefix, "slope-boxplot.png")),
       height = 8, width = 12, units = "in")

# End ----

