## ------------------------------------------------------- ##
                      # Figure Creation
## ------------------------------------------------------- ##
# Written by: Joanna Carey, Lienne Sethna, & Nick J Lyon

# Purpose:
## Make publication-quality figures

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cowplot)

# Clear environment
rm(list = ls())

# Make a folder for exporting graphs
dir.create(path = file.path("figures"), showWarnings = F)

# Load custom functions
for(fxn in dir(path = file.path("tools"), pattern = "fxn_")){
  source(file.path("tools", fxn))
}

## And remove loop index object from environment
rm(list = "fxn")

# Load graph helpers
source(file.path("tools", "flow_graph-helpers.R"))

## ----------------------------------------- ##
# Data Prep ----
## ----------------------------------------- ##

# Identify conserved file stem
file_stem <- "stats-ready_annual_"

# Identify response variable
file_resp <- "Conc_uM"

# Assemble into a full file for each chemical
df_si <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_DSi.csv")))
df_n <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_DIN.csv")))
df_p <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_P.csv")))
df_si.n<- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_Si_DIN.csv")))
df_si.p <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_Si_P.csv")))

# Combine into a single object that loses nothing
df_all <- dplyr::bind_rows(df_si, df_n, df_p, df_si.n, df_si.p)

# Check structure
dplyr::glimpse(df_all)

# Pare down to just needed rows/columns
df_simp <- df_all %>% 
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Do some final tweaks
  dplyr::mutate(
    ## Fix slope direction categories
    slope_direction = dplyr::case_when(
      significance == "marg" ~ "NS",
      is.na(slope_direction) == T ~ "NA",
      T ~ slope_direction),
    ## Assign level order
    slope_direction = factor(slope_direction, levels = c("pos", "neg", "NS", "NA")), 
    ## Bin duration
    duration_bin = ifelse(test = (section_duration <= 5),
                          yes = "short", no = "long") ) %>% 
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical, Year, 
                dplyr::all_of(x = file_resp), significance, slope_direction, duration_bin) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Re-check structure
dplyr::glimpse(df_simp)

# SUSBET FOR TESTING PURPOSES
chem <- "DSi"
TEST <- dplyr::filter(.data = df_simp, chemical == chem)
glimpse(TEST)

# Create a bookmark graph for line direction + P value
ggplot(data = TEST, mapping = aes(x = Year, y = LTER_stream, color = slope_direction)) +
  geom_path(data = TEST[TEST$duration_bin == "long", ], mapping = aes(group = sizer_groups), 
            lwd = 3.5, lineend = 'square') +
  geom_path(data = TEST[TEST$duration_bin == "short", ], mapping = aes(group = sizer_groups), 
            lwd = 3.5, lineend = 'square', alpha = 0.2) +
  scale_color_manual(values = dir_palt) +
  geom_hline(yintercept = lter_ct(data = TEST)$line_positions) +
  labs(x = "Year", y = "Stream") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.9))






# BASEMENT ----

  
  

df_si_v2 <- df_si %>% 
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Do some final tweaks
  dplyr::mutate(
    ## Fix slope direction categories
    slope_direction = dplyr::case_when(
      significance == "marg" ~ "NS",
      is.na(slope_direction) == T ~ "NA",
      T ~ slope_direction),
    ## Bin duration
    duration_bin = ifelse(test = (section_duration <= 5),
                                      yes = "short", no = "long") ) %>% 
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical, Year, 
                dplyr::all_of(x = file_resp), significance, slope_direction, duration_bin) %>%
  # Drop non-unique rows
  dplyr::distinct()

glimpse(df_si_v2)


# Create a bookmark graph for line direction + P value
bookmark_graph(data = df_si_v2, color_var = "slope_direction", colors = dir_palt) +
  theme_bw() +
  theme(axis.text.y = element_blank())



  theme(legend.title = element_blank())

# Read in desired data








# End ----
