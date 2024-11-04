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

# Make a folder for exporting graphs
dir.create(path = file.path("figures"), showWarnings = F)

# Clear environment
rm(list = ls())

## ----------------------------------------- ##
        # Discharge Bookmark Figure ----
## ----------------------------------------- ##

# Load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Read in discharge data
df_q <- read.csv(file = file.path("data", "stats-ready_annual_Discharge_cms_DSi.csv")) %>% 
  # Remove McMurdo streams with incomplete chemical information
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream "))

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
  scale_color_manual(values = dir_palt, breaks = c("pos", "neg", "NS")) +
  # Add lines between streams from different LTERs
  geom_hline(yintercept = streams_per_lter$line_positions) +
  ## Add LTER-specific annotations
  geom_text(x = 1992, y = 1.5, label = "Canada", color = "black", hjust = "left") + 
  annotate(geom = "text", x = 1993, color = "black", angle = 90, hjust = "center",
           y = c(14, 27.5, 35.5, 43, 49.5, 62), 
           label = c("Finland", "GRO", "Krycklan", "MCM", "Norway", "Sweden")) +
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
       height = 9, width = 5, units = "in")

# Tidy environment
rm(list = setdiff(x = ls(), y = c("df_q_simp")))

## ----------------------------------------- ##
        # Chemical Bookmark Figures ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Identify conserved file stem
file_stem <- "stats-ready_annual_"

# Loop across response variables
for(file_resp in c("Conc_uM", "FNConc_uM", "Yield", "FNYield")){
  # file_resp <- "Conc_uM"
  
  # Generate a version of response variable name for plot labels
  resp_lab_v1 <- paste0(gsub(pattern = "_", replacement = " (", x = file_resp))
  resp_lab_v2 <- gsub(pattern = "Conc", replacement = "Concentration", x = resp_lab_v1)
  resp_lab_v3 <- gsub(pattern = "uM", replacement = "uM)", x = resp_lab_v2)
  resp_lab <- gsub(pattern = "FN", replacement = "Flow Normalized ", x = resp_lab_v3)
  
  # Assemble into a full file for each chemical
  df_si <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_DSi.csv")))
  df_n <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_DIN.csv")))
  df_p <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_P.csv")))
  df_si.n<- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_Si_DIN.csv")))
  df_si.p <- read.csv(file = file.path("data", paste0(file_stem, file_resp, "_Si_P.csv")))
  
  # Combine into a single object that loses nothing
  df_chem_all <- dplyr::bind_rows(df_si, df_n, df_p, df_si.n, df_si.p) %>% 
    # Remove McMurdo streams with incomplete chemical information
    dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                      "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                      "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                      "MCM_Priscu Stream "))
  
  # Wrangle this like we wrangled discharge (see above)
  df_chem_simp <- df_chem_all %>% 
    dplyr::arrange(LTER, Stream_Name) %>%
    dplyr::mutate(
      chemical = gsub(pattern = "_", replacement = ":", x = chemical),
      chemical = gsub(pattern = "P", replacement = "DIP", x = chemical),
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
  
  # Count streams / LTER
  (streams_per_lter <- lter_ct(data = df_chem_simp))
  
  # Make a list of the desired number of elements
  chem_bookmarks <- list()
  
  # Loop across chemicals
  for(chem in unique(df_chem_simp$chemical)){
    # chem <- "DSi"
    
    # Message
    message("Creating bookmark graph for ", chem, " (", resp_lab, ")")
    
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
      scale_color_manual(values = dir_palt, breaks = c("pos", "neg", "NS")) +
      # Add lines between streams from different LTERs
      geom_hline(yintercept = streams_per_lter$line_positions) +
      ## Add LTER-specific annotations
      geom_text(x = 1989, y = 1.5, label = "Canada", color = "black", hjust = "left") + 
      annotate(geom = "text", x = 1990, color = "black", angle = 90, hjust = "center",
               y = c(14, 27.5, 35.5, 43, 49.5, 62), 
               label = c("Finland", "GRO", "Krycklan", "MCM", "Norway", "Sweden")) +
      # Customize labels and axis titles
      labs(x = "Year", y = "Stream", 
           title = paste0(chem, " ", resp_lab)) +
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
    if(!chem %in% c("DSi", "Si:DIN")){
      q <- q +
        theme(legend.position = "none")
    }
    
    # Assign the finished bookmark graph to the list
    chem_bookmarks[[chem]] <- q
    
  } # Close chem loop
  
  # Assemble the first figure (non-ratios only)
  cowplot::plot_grid(chem_bookmarks[["DSi"]], chem_bookmarks[["DIN"]], chem_bookmarks[["DIP"]],
                     nrow = 1, labels = "")
  
  # And export it
  ggsave(filename = file.path("figures", paste0("fig_bookmark-chemicals_", 
                                                tolower(file_resp), ".png")),
         height = 9, width = 15, units = "in")
  
  # Assemble & export the second figure (ratios only)
  cowplot::plot_grid(chem_bookmarks[["Si:DIN"]], chem_bookmarks[["Si:DIP"]], nrow = 1)
  ggsave(filename = file.path("figures", paste0("fig_bookmark-chemical-ratios_", 
                                                tolower(file_resp), ".png")),
         height = 9, width = 10, units = "in")
  
} # Close response variable loop

# Tidy environment
rm(list = ls())

## ----------------------------------------- ##
          # Strip Boxplot Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Read in specifically the annual concentration data for the three chemicals
df_conc_si <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DSi.csv"))
df_conc_n <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DIN.csv"))
df_conc_p <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_P.csv"))

# Bind them together
df_conc_all <- dplyr::bind_rows(df_conc_si, df_conc_n, df_conc_p) %>% 
  # Tweak how P (chemical) is written
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>% 
  # Remove McMurdo streams with incomplete chemical information
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream "))

# Want to order boxplots by median silica (within LTER)
df_si_rank <- df_conc_all %>% 
  # Filter to Si only and calculate median / stream
  dplyr::filter(chemical == "DSi") %>% 
  dplyr::group_by(LTER, Stream_Name, LTER_stream) %>% 
  dplyr::summarize(median_si = stats::median(x = Conc_uM, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Rank from highest to lowest
  dplyr::arrange(dplyr::desc(median_si)) %>% 
  # Order within LTER
  dplyr::group_by(LTER) %>% 
  dplyr::mutate(si_rank = seq_along(along.with = median_si)) %>% 
  # Create a new 'lter + stream' column that incorporates this ranking
  dplyr::mutate(site_simp = gsub(pattern = " at", replacement = " ", x = Stream_Name),
                LTER_simp = ifelse(nchar(LTER) <= 4, yes = LTER,
                                   no = stringr::str_sub(LTER, start = 1, end = 4)),
                site_simp = ifelse(nchar(site_simp) <= 14, yes = site_simp,
                                   no = stringr::str_sub(site_simp, start = 1, end = 14)),
                si_rank_char = ifelse(test = (nchar(si_rank) == 1), 
                                      yes = paste0("0", si_rank), no = as.character(si_rank)),
                LTER_stream_ranked = paste0(LTER_simp, "_", si_rank_char, "_", site_simp)) %>% 
  # Drop unwanted columns
  dplyr::select(-dplyr::ends_with("_simp"), -LTER_stream, -median_si, si_rank)

# And do some minor tidying
df_conc <- df_conc_all %>% 
  # Remove an N outlier
  dplyr::filter(chemical != "DIN" | (chemical == "DIN" & Conc_uM <= 250)) %>% 
  # Create factor order of chemicals to get right order of strips
  dplyr::mutate(chemical = factor(chemical, levels = c("DSi", "DIN", "DIP"))) %>% 
  # Attach Si "rankings"
  dplyr::left_join(y = df_si_rank, by = c("LTER", "Stream_Name"))

# Check the structure
dplyr::glimpse(df_conc)

# Count streams / LTER
(streams_per_lter <- lter_ct(data = df_conc))

# Create the boxplot strips
ggplot(df_conc, aes(x = LTER_stream_ranked, y = Conc_uM, fill = LTER)) +
  # Add boxplots (with fill-able points for outliers)
  geom_boxplot(outlier.shape = 21, lwd = 0.2) +
  # Facet into three stacked strips
  facet_grid(chemical ~ ., scales = "free", axes = "all") +
  # Customize color & label titles
  scale_fill_manual(values = lter_palt) +
  labs(x = "Stream", y = "Concentration (uM)") +
  # Add lines between streams from different LTERs
  geom_vline(xintercept = streams_per_lter$line_positions[-7], linetype = 2, color = "gray66") +
  # Add LTER-specific annotations
  geom_text(label = "Can.", x = 1.25, y = 275, angle = 90, hjust = "center") + 
  geom_text(label = "Finland", x = 11.5, y = 300, hjust = "center") + 
  geom_text(label = "GRO", x = 27.5, y = 300, hjust = "center") + 
  geom_text(label = "Krycklan", x = 35.5, y = 300, hjust = "center") + 
  geom_text(label = "MCM", x = 43, y = 300, hjust = "center") + 
  geom_text(label = "Norway", x = 49.5, y = 300, hjust = "center") + 
  geom_text(label = "Sweden", x = 62, y = 300, hjust = "center") + 
  # Customize the legend
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15))

# Export graph
ggsave(filename = file.path("figures", "fig_boxplot-chemicals_conc_um.png"),
       height = 7, width = 10, units = "in")

# Tidy environment
rm(list = ls())

## ----------------------------------------- ##
    # Strip Boxplot Figure - Zoom In ----
## ----------------------------------------- ##

# NOTE
## Most of this code is the same as the other 'strip boxplot' figure
## Comments have been pared down because of this

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Read in specifically the annual concentration data for the three chemicals
df_conc_si <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DSi.csv"))
df_conc_n <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DIN.csv"))
df_conc_p <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_P.csv"))

# Combine chemical datasets
df_conc_all <- dplyr::bind_rows(df_conc_si, df_conc_n, df_conc_p) %>% 
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>% 
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream "))

# Do preliminary wrangling to define boxplot order
df_si_rank <- df_conc_all %>% 
  dplyr::filter(chemical == "DSi") %>% 
  dplyr::group_by(LTER, Stream_Name, LTER_stream) %>% 
  dplyr::summarize(median_si = stats::median(x = Conc_uM, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(dplyr::desc(median_si)) %>% 
  dplyr::group_by(LTER) %>% 
  dplyr::mutate(si_rank = seq_along(along.with = median_si)) %>% 
  dplyr::mutate(site_simp = gsub(pattern = " at", replacement = " ", x = Stream_Name),
                LTER_simp = ifelse(nchar(LTER) <= 4, yes = LTER,
                                   no = stringr::str_sub(LTER, start = 1, end = 4)),
                site_simp = ifelse(nchar(site_simp) <= 14, yes = site_simp,
                                   no = stringr::str_sub(site_simp, start = 1, end = 14)),
                si_rank_char = ifelse(test = (nchar(si_rank) == 1), 
                                      yes = paste0("0", si_rank), no = as.character(si_rank)),
                LTER_stream_ranked = paste0(LTER_simp, "_", si_rank_char, "_", site_simp)) %>% 
  dplyr::select(-dplyr::ends_with("_simp"), -LTER_stream, -median_si, si_rank)

# Minor pre-graphing wrangling
df_conc <- df_conc_all %>% 
  dplyr::filter(chemical != "DIN" | (chemical == "DIN" & Conc_uM <= 250)) %>% 
  dplyr::mutate(chemical = factor(chemical, levels = c("DSi", "DIN", "DIP"))) %>% 
  dplyr::left_join(y = df_si_rank, by = c("LTER", "Stream_Name"))

# Do some subsetting to differentiate this graph from the other boxplot!
df_conc_sub <- df_conc %>% 
  # Filter to desired per-chemical thresholds
  dplyr::filter(
    (chemical == "DSi" & LTER %in% c("MCM", "NIVA", "Swedish Goverment") & Conc_uM < 100) |
      (chemical == "DIN" & LTER %in% c("GRO", "Krycklan", "MCM", 
                                       "NIVA", "Swedish Goverment") & Conc_uM < 50) |
      (chemical == "DIP" & LTER %in% c("Swedish Goverment") & Conc_uM < 0.75) )

# Check the structure
dplyr::glimpse(df_conc_sub)

# Count streams / LTER
(streams_per_lter <- lter_ct(data = df_conc_sub)[-5,])

# Create the boxplot strips *with the "zoomed in" data object*
ggplot(df_conc_sub, aes(x = LTER_stream_ranked, y = Conc_uM, fill = LTER)) +
  geom_boxplot(outlier.shape = 21, lwd = 0.2) +
  facet_grid(chemical ~ ., scales = "free", axes = "all") +
  scale_fill_manual(values = lter_palt) +
  labs(x = "Stream", y = "Concentration (uM)") +
  geom_vline(xintercept = streams_per_lter$line_positions[-7], linetype = 2, color = "gray66") +
  geom_text(label = "GRO", x = 3.5, y = 95, hjust = "center") + 
  geom_text(label = "Krycklan", x = 11, y = 95, hjust = "center") + 
  geom_text(label = "MCM", x = 19, y = 95, hjust = "center") + 
  geom_text(label = "Norway", x = 25.5, y = 85, hjust = "center") + 
  geom_text(label = "Sweden", x = 39, y = 95, hjust = "center") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 15))

# Export graph
ggsave(filename = file.path("figures", "fig_boxplot-chemicals-zoom_conc_um.png"),
       height = 7, width = 10, units = "in")

# Tidy environment
rm(list = ls())

## ----------------------------------------- ##
  # 'Pick Up Sticks' DSi % Change Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Read in DSi data
si_v1 <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DSi.csv"))

# Process this as needed
si_v2 <- si_v1 %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical,
                mean_response, percent_change,
                dplyr::starts_with(c("slope_", "mean_"))) %>% 
  dplyr::select(-slope_estimate, -slope_direction, -slope_std_error,
                -dplyr::contains(c("_FNConc_", "_NO3_", "_DIN_", "_NH4_",
                                   "_NOx_", "_Si.DIN_", "_Si.P_"))) %>% 
  # Change certain column names to be more informative
  dplyr::rename(mean_si_conc = mean_response,
                perc.change_si_conc = percent_change) %>% 
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

# Check structure
dplyr::glimpse(si_v2)
## tibble::view(si_v2)

## Net Primary Productivity (NPP)
perc_npp <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
            exp_var = "slope_npp_kgC.m2.year", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "NPP (kg C/m2/year) Annual Change") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.12, 0.825),
        legend.direction = "vertical",
        legend.background = element_blank()); perc_npp
## Precipitation
perc_ppt <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
            exp_var = "slope_precip_mm.per.day", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "Precipitation (mm/day) Annual Change") +
  theme(legend.position = "none"); perc_ppt
## Snow (Proportion Area)
perc_snow <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_snow_max.prop.area", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "Snow (Max Proportion Area) Annual Change") +
  theme(legend.position = "none"); perc_snow
## Temperature
perc_temp <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_temp_degC", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "Temperature (C) Annual Change") +
  theme(legend.position = "none"); perc_temp
## Phosphorus concentration
perc_pconc <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_P_Conc_uM", sig = "main") +
  labs(y = "DSi Concentration (% Change)",
       x = "P Concentration (uM) Annual Change") +
  theme(legend.position = "none"); perc_pconc
## Discharge
perc_disc <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                          exp_var = "slope_Discharge_cms", sig = "NS") +
  labs(y = "DSi Concentration (% Change)",
       x = "Discharge (cms) Annual Change") +
  theme(legend.position = "none"); perc_disc
## LTER boxplots
perc_box <- ggplot(si_v2, aes(x = LTER, y = perc.change_si_conc, fill = LTER)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = lter_palt) +
  scale_x_discrete(labels = c("Canada", "Finland", "GRO", "Krycklan", 
                              "MCM", "Norway", "Sweden")) +
  labs(y = "DSi Concentration (% Change)", x = "LTER") +
  geom_text(label = "NS", x = 1, y = 14, hjust = "center") + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        legend.position = "none"); perc_box

# Assemble into grid of plots
cowplot::plot_grid(perc_npp, perc_ppt, perc_snow, perc_box, perc_temp, perc_pconc, perc_disc,
                   nrow = 2, labels = "AUTO")

# Export as a figure
ggsave(filename = file.path("figures", "fig_sticks_si_perc-change.png"),
       height = 10, width = 15, units = "in")

# Tidy environment
rm(list = ls())

## ----------------------------------------- ##
  # 'Pick Up Sticks' Mean DSi Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Read in DSi data
si_v1 <- read.csv(file = file.path("data", "stats-ready_annual_Conc_uM_DSi.csv"))

# Process this as needed
si_v2 <- si_v1 %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical,
                mean_response, percent_change,
                dplyr::starts_with(c("slope_", "mean_"))) %>% 
  dplyr::select(-slope_estimate, -slope_direction, -slope_std_error,
                -dplyr::contains(c("_FNConc_", "_NO3_", "_DIN_", "_NH4_",
                                   "_NOx_", "_Si.DIN_", "_Si.P_"))) %>% 
  # Change certain column names to be more informative
  dplyr::rename(mean_si_conc = mean_response,
                perc.change_si_conc = percent_change) %>% 
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

# Check structure
dplyr::glimpse(si_v2)
## tibble::view(si_v2)

## Net Primary Productivity (NPP)
avg_npp <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                        exp_var = "mean_npp_kgC.m2.year", sig = "main") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean NPP (kg C/m2/year)") +
  theme(legend.position = "none"); avg_npp
## Precipitation
avg_ppt <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                        exp_var = "mean_precip_mm.per.day", sig = "ixn") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Precipitation (mm/day)") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.direction = "vertical",
        legend.background = element_blank()); avg_ppt
## Snow (Proportion Area)
avg_snow <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_snow_max.prop.area", sig = "ixn") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Snow (Max Proportion Area)") +
  theme(legend.position = "none"); avg_snow
## Temperature
avg_temp <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_temp_degC", sig = "main") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Temperature (C)") +
  theme(legend.position = "none"); avg_temp
## Phosphorus concentration
avg_pconc <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                          exp_var = "mean_P_Conc_uM", sig = "main") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean P Concentration (uM)") +
  theme(legend.position = "none"); avg_pconc
## Discharge
avg_disc <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_Discharge_cms", sig = "NS") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Discharge (cms)") +
  theme(legend.position = "none"); avg_disc
## LTER boxplots
avg_box <- ggplot(si_v2, aes(x = LTER, y = mean_si_conc, fill = LTER)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = lter_palt) +
  scale_x_discrete(labels = c("Canada", "Finland", "GRO", "Krycklan", 
                              "MCM", "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)", x = "LTER") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        legend.position = "none"); avg_box

# Assemble into grid of plots
cowplot::plot_grid(avg_npp, avg_ppt, avg_snow, avg_box, avg_temp, avg_pconc, avg_disc,
                   nrow = 2, labels = "AUTO")

# Export as a figure
ggsave(filename = file.path("figures", "fig_sticks_si_mean.png"),
       height = 10, width = 15, units = "in")

# End ----
