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
        # Discharge Bookmark Figure ----
## ----------------------------------------- ##

# Read in discharge data
df_q <- read.csv(file = file.path("data", "stats-ready_annual_Discharge_cms_DSi.csv"))

# Simplify this object
df_q_simp <- df_q %>% 
  # Arrange by LTER and site
  dplyr::arrange(LTER, Stream_Name) %>%
  # Do some final tweaks
  dplyr::mutate(
    ## Fix slope direction categories
    slope_direction = dplyr::case_when(
      significance == "NS" ~ "NS",
      significance == "marg" ~ "NS",
      is.na(slope_direction) == T ~ "NA",
      T ~ slope_direction),
    ## Assign level order
    slope_direction = factor(slope_direction, levels = names(dir_palt)), 
    ## Bin duration
    duration_bin = ifelse(test = (section_duration <= 5),
                          yes = "short", no = "long") ) %>% 
  # Pare down to only needed columns
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical, Year, 
                Discharge_cms, significance, slope_direction, duration_bin) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(df_q_simp)

# Count streams / LTER
(streams_per_lter <- lter_ct(data = df_q_simp))

# Create a bookmark graph for line direction alone (but only when significant)
ggplot(data = df_q_simp, mapping = aes(x = Year, y = LTER_stream, color = slope_direction)) +
  # Add paths for 'long' duration SiZer chunks
  geom_path(data = df_q_simp[df_q_simp$duration_bin == "long", ], 
            mapping = aes(group = sizer_groups), 
            lwd = 3.5, lineend = 'square') +
  # Add *semi-transparent* paths for short duration chunks
  geom_path(data = df_q_simp[df_q_simp$duration_bin == "short", ], 
            mapping = aes(group = sizer_groups), 
            lwd = 3.5, lineend = 'square', alpha = 0.5) +
  # Manually define colors
  scale_color_manual(values = dir_palt) +
  # Add lines between streams from different LTERs
  geom_hline(yintercept = streams_per_lter$line_positions) +
  ## Add LTER-specific annotations
  geom_text(x = 1992, y = 1.5, label = "Canada", color = "black", hjust = "left") + 
  annotate(geom = "text", x = 1993, color = "black", angle = 90, hjust = "center",
           y = c(14, 27.5, 35.5, 46.5, 56.5, 69), 
           label = c("Finnish Environ. Institute", "GRO", "Krycklan", "McMurdo", "NIVA", "Swedish Gov't")) +
  # Customize labels and axis titles
  labs(x = "Year", y = "Stream", title = "Discharge (cms)") +
  # Modify theme elements for preferred aesthetics
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.425, 0.88))

# Export graph
ggsave(filename = file.path("figures", "fig_bookmark-discharge.png"),
       height = 8, width = 5, units = "in")

## ----------------------------------------- ##
        # Chemical Bookmark Figures ----
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
df_chem_all <- dplyr::bind_rows(df_si, df_n, df_p, df_si.n, df_si.p)

# Wrangle this like we wrangled discharge (see above)
df_chem_simp <- df_chem_all %>% 
  dplyr::arrange(LTER, Stream_Name) %>%
  dplyr::mutate(
    slope_direction = dplyr::case_when(
      significance == "NS" ~ "NS",
      significance == "marg" ~ "NS",
      is.na(slope_direction) == T ~ "NA",
      T ~ slope_direction),
    slope_direction = factor(slope_direction, levels = c("pos", "neg", "NS", "NA")), 
    duration_bin = ifelse(test = (section_duration <= 5),
                          yes = "short", no = "long") ) %>% 
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical, Year, 
                dplyr::all_of(x = file_resp), significance, slope_direction, duration_bin) %>%
  dplyr::distinct()

# Re-check structure
dplyr::glimpse(df_chem_simp)

# Count streams / LTER
(streams_per_lter <- lter_ct(data = df_chem_simp))

# Make a list of the desired number of elements
chem_bookmarks <- list()

# Loop across chemicals
for(chem in unique(df_chem_simp$chemical)){
  
  # Message
  message("Creating bookmark graph for ", chem)
  
  # Subset data
  df_chem_sub <- dplyr::filter(df_chem_simp, chemical == chem)
  
  # Identify any streams that don't have data for this chemical
  df_missing <- df_q_simp %>% 
    dplyr::filter(!LTER_stream %in% unique(df_chem_sub$LTER_stream)) %>% 
    dplyr::mutate(significance = "NA", slope_direction = "NA")
    
  # Re-attach any streams that were dropped (we want the same number of 'rows' in all graphs)
  df_chem <- dplyr::bind_rows(df_chem_sub, df_missing)
  
  # Create the bookmark graph 
  q <- ggplot(data = df_chem, mapping = aes(x = Year, y = LTER_stream, 
                                                color = slope_direction)) +
    # Add paths for 'long' duration SiZer chunks
    geom_path(data = df_chem[df_chem$duration_bin == "long", ], 
              mapping = aes(group = sizer_groups), 
              lwd = 3.5, lineend = 'square') +
    # Add *semi-transparent* paths for short duration chunks
    geom_path(data = df_chem[df_chem$duration_bin == "short", ], 
              mapping = aes(group = sizer_groups), 
              lwd = 3.5, lineend = 'square', alpha = 0.5) +
    # Manually define colors
    scale_color_manual(values = dir_palt) +
    # Add lines between streams from different LTERs
    geom_hline(yintercept = streams_per_lter$line_positions) +
    ## Add LTER-specific annotations
    geom_text(x = 1992, y = 1.5, label = "Canada", color = "black", hjust = "left") + 
    annotate(geom = "text", x = 1990, color = "black", angle = 90, hjust = "center",
             y = c(14, 27.5, 35.5, 46.5, 56.5, 69), 
             label = c("Finnish Environ. Institute", "GRO", "Krycklan", 
                       "McMurdo", "NIVA", "Swedish Gov't")) +
    # Customize labels and axis titles
    labs(x = "Year", y = "Stream", 
         title = paste0(chem, " ", gsub(pattern = "_", replacement = " (", x = file_resp), ")")) +
    # Modify theme elements for preferred aesthetics
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    theme(panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.line = element_line(color = "black"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.225, 0.88))

  # Remove the legend from all but specified chemicals
  if(!chem %in% c("DSi", "Si_DIN")){
    q <- q +
      theme(legend.position = "none")
  }
  
  # Assign the finished bookmark graph to the list
  chem_bookmarks[[chem]] <- q
  
}

# Assemble the first figure (non-ratios only)
cowplot::plot_grid(chem_bookmarks[["DSi"]], chem_bookmarks[["DIN"]], chem_bookmarks[["P"]],
                   nrow = 1, labels = "")

# And export it
ggsave(filename = file.path("figures", "fig_bookmark-chemicals.png"),
       height = 8, width = 15, units = "in")

# Assemble & export the second figure (ratios only)
cowplot::plot_grid(chem_bookmarks[["Si_DIN"]], chem_bookmarks[["Si_P"]], nrow = 1)
ggsave(filename = file.path("figures", "fig_bookmark-chemical-ratios.png"),
       height = 8, width = 10, units = "in")



# End ----
