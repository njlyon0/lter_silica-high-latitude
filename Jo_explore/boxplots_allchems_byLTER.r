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
              # Data Prep ----
## ----------------------------------------- ##

# Identify the filenames
file_name1 <- "stats-ready_nodriversannual_Conc_uM_DSi_bw5.csv"
file_name2 <- "stats-ready_nodriversannual_Conc_uM_DIN_bw5.csv"
file_name3 <- "stats-ready_nodriversannual_Conc_uM_P_bw5.csv"
file_name4 <- "stats-ready_nodriversannual_Conc_uM_Si_DIN_bw5.csv"
file_name5 <- "stats-ready_nodriversannual_Conc_uM_Si_P_bw5.csv"

# Grab the desired data files
full_Si <- read.csv(file = file.path("tidy_data", file_name1))
full_N <- read.csv(file = file.path("tidy_data", file_name2))
full_P <- read.csv(file = file.path("tidy_data", file_name3))
full_Si_N <- read.csv(file = file.path("tidy_data", file_name4))
full_Si_P <- read.csv(file = file.path("tidy_data", file_name5))

combo_full<-bind_rows(full_Si, full_N, full_P, full_Si_N, full_Si_P)

# Glimpse it
dplyr::glimpse(combo_full)

# Do some processing
full_df <- combo_full %>% 
  #remove Canada b/c no ratios
  dplyr::filter(!LTER %in% c("Canada")) %>%
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Swedish Government", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>%
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>%
  dplyr::mutate(chemical = gsub(pattern = "_", replacement = ":", x = chemical)) %>%
  # Make both 'direction + X' columns into factors so we can pick an informative order
  dplyr::mutate(dir_sig = factor(dir_sig, levels = c("pos-sig", "pos-marg", 
                                                     "neg-marg", "neg-sig", "NA", "NS")),
                dir_fit = factor(dir_fit, 
                                 levels = c("pos-great", "pos-good", "pos-fine", "pos-bad",
                                            "neg-bad", "neg-fine", "neg-good", "neg-great",
                                            "NA", "NS")),
                chemical = factor(chemical, 
                                 levels = c("DSi", "DIN", "DIP", "Si:DIN", "Si:DIP")))

# Check its structure
dplyr::glimpse(full_df)

# Make a data object with only the columns that we'll want
core_df <- full_df %>%
  # Arrange by LTER, site, and month - arranges rows
  dplyr::arrange(chemical, LTER, Stream_Name, Month) %>%
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
  # Keep only significant slopes - including marginal
  #dplyr::filter(significance %in% c("sig", "marg")) %>%
  # Keep only significant slopes - excluding marginal
  dplyr::filter(significance %in% c("sig")) %>%
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>%
  # Arrange by LTER and site
  dplyr::arrange(chemical, LTER, Stream_Name) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check it out
dplyr::glimpse(sig_only)

## ----------------------------------------- ##
              # Plotting Prep ----
## ----------------------------------------- ##

# Grab useful information for informative file names for these graphs
resp <- gsub(pattern = "_mgL|_uM|_10_6kg_yr|_10_6kmol_yr|_kmol_yr_km2|_kmol_yr|_kg_yr", 
             replacement = "", x = names(full_df)[11])
## Note response identification is dependent upon column order!
names(full_df)

# Assemble this into a file prefix
(file_prefix <- paste0("_", resp, "_"))

chem_palt <- c("DSi" = "#d62828", "DIN" = "blue", "DIP" = "#fee440", "Si:DIN" = "#7209b7", "Si:DIP" = "#008000")
chem_palt2 <- c("DSi" = "#ef233c", "DIN" = "#ffffff", "DIP" = "blue", "Si:DIN" = "#fb6f92", "Si:DIP" = "#7209b7")
#https://coolors.co/palettes/trending

## ----------------------------------------- ##
        # Percent Change Boxplots ----
## ----------------------------------------- ##

# Make graph
ggplot(sig_only, aes(x = chemical, y = percent_change, fill = chemical)) +
  #geom_violin() +
  geom_boxplot(outlier.shape = 21) +
  #geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  scale_fill_manual(values = chem_palt2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "", y = "Percent Change",
       title = paste("Significant changes in", resp)) +
  theme_classic()+
  theme(panel.spacing = unit(2, "lines"), legend.position = "none",
        strip.background = element_blank())

# Export it
ggsave(filename = file.path("graphs", paste0(file_prefix, "perc-change-boxplot_allelements.png")),
       height = 6, width = 8, units = "in")



## ----------------------------------------- ##
# Slope Boxplots - these don't plot well because ratio values so different
## ----------------------------------------- ##
names(sig_only)

# Make graph
ggplot(sig_only, aes(x = chemical, y = slope_estimate, fill = chemical)) +
  #geom_violin() +
  geom_boxplot(outlier.shape = 21) +
  #geom_jitter(width = 0.2, alpha = 0.5, pch = 21) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'black', linetype = 2) +
  scale_fill_manual(values = chem_palt2) +
  # Facet by LTER
  facet_wrap( ~ LTER, scales = "free_y", nrow = 5, strip.position = "right") +
  # Custom theming / labels
  labs(x = "", y = "Rate of Change (uM yr-1)",
       title = paste("Significant changes in", resp)) +
  theme_classic()+
  theme(panel.spacing = unit(2, "lines"), legend.position = "none",
        strip.background = element_blank())

# Export it
#ggsave(filename = file.path("graphs", paste0(file_prefix, "perc-change-boxplot_allelements.png")),
       height = 6, width = 8, units = "in")


              
###============================
#summary stats on SiZer Ouputs
###============================
              
sig_only %>%
  dplyr::group_by(chemical, LTER, dir_sig) %>% #add in stream if want to do it by stream
  dplyr::summarize(ct=n(), 
                   avg_perChange = mean(percent_change),
                   sd_perChange = sd(percent_change)) #not working b/c this column are factors?

sig_only %>%
  dplyr::group_by(dir_sig) %>%
  dplyr::summarize(ct=n()) #n results in same thing as length. #be careful if number rows proxy looking for
#can do length(Year) to double check. n function wants nothing in the parenthese

#group by lter, chemical and stream and count number of unique sizer groups
sig_only %>%
  dplyr::group_by(chemical, LTER) %>%
  summarize(sizer_ct = length(unique(sizer_groups))) #this will help identify breakpoints and inflection points



# End ----

