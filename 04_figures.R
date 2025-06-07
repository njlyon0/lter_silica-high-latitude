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
librarian::shelf(tidyverse, cowplot, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "figures"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ----------------------------------------- ##
        # Discharge Bookmark Figure ----
## ----------------------------------------- ##

# Load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Read in discharge data
df_q <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Discharge_cms_DSi.csv")) %>% 
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
ggplot(data = df_q_simp, aes(x = Year, y = LTER_stream)) +
  # Add points with underlying lines for each section
  geom_path(aes(group = sizer_groups, color = slope_direction), 
            lwd = 2.5, alpha = 0.6) +
  geom_point(aes(group = sizer_groups, fill = slope_direction, 
                 shape = slope_direction), size = 2) +
  geom_point(data = df_q_simp[df_q_simp$slope_direction != "NA", ],
             aes(shape = slope_direction), color = "white", size = 2, fill = NA) +
  # Manually specify point/line colors and point shapes
  scale_color_manual(values = dir_palt, breaks = c("pos", "neg", "NS", "NA"), 
                     guide = "none") +
  scale_fill_manual(values = dir_palt, breaks = c("pos", "neg", "NS", "NA")) +
  scale_shape_manual(values = dir_shps, breaks = c("pos", "neg", "NS", "NA")) +
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
  theme_bookmark +
  theme(legend.position = "inside",
        legend.position.inside = c(0.425, 0.88))

# Export graph
ggsave(filename = file.path("graphs", "figures", "fig_bookmark-discharge.png"),
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
file_stem <- "stats-ready_annual"

# Loop across response variables
for(file_resp in c("Conc_uM", "FNConc_uM", "Yield", "FNYield")){
  # file_resp <- "Conc_uM"
  
  # Generate a version of response variable name for plot labels
  resp_lab_v1 <- paste0(gsub(pattern = "_", replacement = " (", x = file_resp))
  resp_lab_v2 <- gsub(pattern = "Conc", replacement = "Concentration", x = resp_lab_v1)
  resp_lab_v3 <- gsub(pattern = "uM", replacement = "uM)", x = resp_lab_v2)
  resp_lab <- gsub(pattern = "FN", replacement = "Flow Normalized ", x = resp_lab_v3)
  
  # Read in all chemical for this response
  df_chem_all <- purrr::map(.x = dir(path = file.path("data", file_stem),
                                   pattern = paste0("_", file_resp, "_")),
                          .f = ~ read.csv(file = file.path("data", file_stem, .x))) %>% 
    # Stack them vertically
    purrr::list_rbind(x = .) %>% 
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
    q <- ggplot(data = df_chem, mapping = aes(x = Year, y = LTER_stream)) +
      # Add points with underlying lines for each section
      geom_path(aes(group = sizer_groups, color = slope_direction), 
                lwd = 2.5, alpha = 0.6) +
      geom_point(aes(group = sizer_groups, fill = slope_direction, 
                     shape = slope_direction), size = 2) +
      geom_point(data = df_chem[df_chem$slope_direction != "NA", ],
                 aes(shape = slope_direction), color = "white", size = 2, fill = NA) +
      # Manually specify point/line colors and point shapes
      scale_color_manual(values = dir_palt, breaks = c("pos", "neg", "NS", "NA"), 
                         guide = "none") +
      scale_fill_manual(values = dir_palt, breaks = c("pos", "neg", "NS", "NA")) +
      scale_shape_manual(values = dir_shps, breaks = c("pos", "neg", "NS", "NA")) +
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
      theme_bookmark +
      theme(legend.position = "inside",
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
  ggsave(filename = file.path("graphs", "figures", paste0("fig_bookmark-chemicals_", 
                                                tolower(file_resp), ".png")),
         height = 9, width = 15, units = "in")
  
  # Assemble & export the second figure (ratios only)
  cowplot::plot_grid(chem_bookmarks[["Si:DIN"]], chem_bookmarks[["Si:DIP"]], nrow = 1)
  ggsave(filename = file.path("graphs", "figures", paste0("fig_bookmark-chemical-ratios_", 
                                                tolower(file_resp), ".png")),
         height = 9, width = 10, units = "in")
  
} # Close response variable loop

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
          # Strip Boxplot Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Read in specifically the annual concentration data for the three chemicals
df_conc_si <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))
df_conc_n <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DIN.csv"))
df_conc_p <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_P.csv"))

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
  theme_facetbox +
  theme(axis.text.x = element_blank())

# Export graph
ggsave(filename = file.path("graphs", "figures", "fig_boxplot-chemicals_conc_um.png"),
       height = 7, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

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
df_conc_si <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))
df_conc_n <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DIN.csv"))
df_conc_p <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_P.csv"))

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

# Loop across chemicals
for(focal_chem in unique(df_conc_sub$chemical)){
  # focal_chem <- "DSi"
  
  # Subset data again to just this chemical
  df_conc_sub2 <- df_conc_sub %>% 
    dplyr::filter(chemical == focal_chem)
  
  # Count 'streams per LTER'
  streams_per_lter <- lter_ct(data = df_conc_sub2) %>% 
    dplyr::filter(LTER != "Swedish Goverment")
  
  # Create the boxplot strips *with the "zoomed in" data object*
  ggplot(df_conc_sub2, aes(x = LTER_stream_ranked, y = Conc_uM, fill = LTER)) +
    geom_boxplot(outlier.shape = 21, lwd = 0.2) +
    facet_grid(chemical ~ ., scales = "free", axes = "all") +
    scale_fill_manual(values = lter_palt) +
    labs(x = "Stream", y = "Concentration (uM)") +
    geom_vline(xintercept = streams_per_lter$line_positions,
               linetype = 2, color = "gray66") +
    theme_facetbox +
    theme(strip.text = element_blank(),
          axis.text.x = element_blank())
  
  # Generate local file name
  focal_out <- paste0("fig_boxplot-chemicals-", tolower(focal_chem), "-zoom_conc_um.png")
  
  # Export locally
  ggsave(filename = file.path("graphs", "figures", focal_out),
         height = 2, width = 6, units = "in")
}

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
  # 'Pick Up Sticks' DSi % Change Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Read in DSi data
si_v1 <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))

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

## Evaporation
perc_ET <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
            exp_var = "slope_evapotrans_kg.m2", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "Evapotranspiration (kg/m2) Annual Change") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.direction = "vertical",
        axis.text = element_text(color = "black"),
        legend.background = element_blank()); perc_ET
## Precipitation
perc_ppt <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
            exp_var = "slope_precip_mm.per.day", sig = "main") +
  labs(y = "DSi Concentration (% Change)",
       x = "Precipitation (mm/day) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_ppt
## Snow (Proportion Area)
perc_snow <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_snow_max.prop.area", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "Snow (Max Proportion Area) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_snow
## Temperature
perc_temp <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_temp_degC", sig = "ixn") +
  labs(y = "DSi Concentration (% Change)",
       x = "Temperature (C) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_temp
## Phosphorus concentration
perc_pconc <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_P_Conc_uM", sig = "main") +
  labs(y = "DSi Concentration (% Change)",
       x = "P Concentration (uM) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_pconc
## Discharge
perc_disc <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                          exp_var = "slope_Discharge_cms", sig = "NS") +
  labs(y = "DSi Concentration (% Change)",
       x = "Discharge (cms) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_disc
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
        axis.text = element_text(color = "black"),
        legend.position = "none"); perc_box

# Assemble into grid of plots
cowplot::plot_grid(perc_ET, perc_ppt, perc_snow, perc_box, perc_temp, perc_pconc, perc_disc,
                   nrow = 2, labels = "AUTO")

# Export as a figure
ggsave(filename = file.path("graphs", "figures", "fig_sticks_si_perc-change.png"),
       height = 10, width = 15, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
  # 'Pick Up Sticks' Mean DSi Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Read in DSi data
si_v1 <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))

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

## ET
avg_ET <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                        exp_var = "mean_evapotrans_kg.m2", sig = "ixn") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Evapotranspiration (kg/m2)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_ET
## Precipitation
avg_ppt <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                        exp_var = "mean_precip_mm.per.day", sig = "ixn") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Precipitation (mm/day)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_ppt
## Snow (Proportion Area)
avg_snow <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_snow_max.prop.area", sig = "ixn") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Snow (Max Proportion Area)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_snow
## Temperature
avg_temp <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_temp_degC", sig = "ixn") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Temperature (C)") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.8),
        legend.direction = "vertical",
        axis.text = element_text(color = "black"),
        legend.background = element_blank()); avg_temp
## Phosphorus concentration
avg_pconc <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                          exp_var = "mean_P_Conc_uM", sig = "main") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean P Concentration (uM)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_pconc
## Discharge
avg_disc <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_Discharge_cms", sig = "NS") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Discharge (cms)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_disc
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
        axis.text = element_text(color = "black"),
        legend.position = "none"); avg_box

# Assemble into grid of plots
cowplot::plot_grid(avg_ET, avg_ppt, avg_snow, avg_box, avg_temp, avg_pconc, avg_disc,
                   nrow = 2, labels = "AUTO")

# Export as a figure
ggsave(filename = file.path("graphs", "figures", "fig_sticks_si_mean.png"),
       height = 10, width = 15, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Monthly Concentration (FN v. Not) Boxplots ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Loop across chemicals
for(focal_chem in c("DSi", "DIN", "P")){
  # focal_chem <- "DIN"
  
  # Progress message
  message("Creating normalization comparison graphs for: ", focal_chem)

  # Identify desired files
  # Read in necessary data file(s)
  ## Not normalized
  month_v1 <- read.csv(file = file.path("data", "stats-ready_monthly", 
                                        paste0("stats-ready_monthly_Conc_uM_",
                                               focal_chem, ".csv"))) %>% 
    dplyr::mutate(normalize = "Not")
  ## Flow-Normalized (FN)
  fn_month_v1 <- read.csv(file = file.path("data", "stats-ready_monthly", 
                                           paste0("stats-ready_monthly_FNConc_uM_",
                                                  focal_chem, ".csv"))) %>% 
    dplyr::mutate(normalize = "FN")
  
  # Combine & do some minor wrangling
  month_v2 <- dplyr::bind_rows(month_v1, fn_month_v1) %>%
    # Make synthetic column for aesthetic mapping
    dplyr::mutate(norm_chem = paste0(normalize, "_", chemical)) %>% 
    # Filter to only significant periods
    dplyr::filter(test_p_value < 0.05) %>% 
    # Simplify (some) LTER names
    dplyr::mutate(LTER = dplyr::case_when(
      LTER == "Finnish Environmental Institute" ~ "Finland",
      LTER == "Swedish Goverment" ~ "Sweden",
      LTER == "NIVA" ~ "Norway",
      T ~ LTER))
  
  # Make a prettier version of P
  if(focal_chem == "P"){ 
    pretty_chem <- "DIP"
    month_v2$chemical <- pretty_chem
    month_v2$norm_chem <- gsub(pattern = "P", replacement = pretty_chem, 
                               x = month_v2$norm_chem) 
    } else { pretty_chem <- focal_chem }
  
  # Conditional vertical placement of legend for each element
  if(focal_chem == "P"){
    leg_y <- 0.48
  } else if(focal_chem == "DIN"){
    leg_y <- 0.45
  } else if(focal_chem == "DSi"){
    leg_y <- 0.43
  }
  
  # Make figure
  ggplot(month_v2, mapping = aes(x = as.factor(Month), y = percent_change, 
                                 fill = norm_chem)) +
    geom_boxplot(outlier.shape = 21) +
    geom_hline(yintercept = 0, linetype = 2) +
    facet_grid(LTER ~ ., scales = "free") +
    scale_fill_manual(values = normchem_palt) +
    labs(y = paste0("Significant ", pretty_chem, " Concentration Change (%)"), 
         x = "LTER", fill = "Normalize_Chem.") +
    theme(panel.background = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.6, leg_y),
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 13, color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          strip.background = element_blank(),
          strip.text = element_text(size = 11),
          panel.spacing = unit(x = 2, units = "lines"))
  
  # Export as a figure
  ggsave(filename = file.path("graphs", "figures", 
                              paste0("fig_monthly-boxplot-", tolower(pretty_chem), 
                                     "-conc-vs-fnconc.png")),
         height = 12, width = 8, units = "in")
}

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Conc. Violins by LTER & Chemical (Mean) ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Read in necessary data file(s)
df_conc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                               pattern = "_Conc_uM_"),
                      .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  # Stack them vertically
  purrr::list_rbind(x = .) %>% 
  # Drop Canada (lacks many chemicals)
  dplyr::filter(!LTER %in% c("Canada")) %>% 
  # Keep only significant slopes - excluding marginal
  # dplyr::filter(significance %in% c("sig")) %>% # excluding for now
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>% 
  # Pare down to only desired columns
  dplyr::select(sizer_groups, LTER, Stream_Name, chemical, mean_response, percent_change) %>% 
  # Standardize names of LTERs / chemicals
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Sweden", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>%
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>%
  dplyr::mutate(chemical = gsub(pattern = "_", replacement = ":", x = chemical)) %>% 
  # Order chemicals
  dplyr::mutate(chemical = factor(chemical, levels = c("DIN", "Si:DIN", "DSi", 
                                                       "Si:DIP", "DIP")))

# Check structure
dplyr::glimpse(df_conc)

# Summarize as well for mean +/- SE bars
df_summary <- supportR::summary_table(data = df_conc, response = "mean_response",
                                      groups = c("LTER", "chemical"))

# Check that out
dplyr::glimpse(df_summary)

# Make a list to store chemical-specific panels
panel_list <- list()

# Assemble graph panels
for(focal_chem in levels(df_conc$chemical)){
  # focal_chem <- "DIN"
  
  # Processing message
  message("Making graph panel for ", focal_chem)
  
  # Subset data
  focal_conc <- dplyr::filter(df_conc, chemical == focal_chem)
  focal_summary <- dplyr::filter(df_summary, chemical == focal_chem)
  
  # Generate desired graph
  focal_panel <- ggplot(focal_conc, aes(x = chemical, y = mean_response)) +
    geom_jitter(aes(color = chemical), width = 0.15, alpha = 0.25) +
    geom_violin(aes(fill = chemical), alpha = 0.2) +
    # Facet by LTER & chemical
    facet_grid(LTER ~ chemical, scales = "free_y") +
    # Add averaged points with SE bars
    geom_point(data = focal_summary, aes(x = chemical, y = mean, fill = chemical), 
               size = 3, shape = 21) +
    geom_errorbar(data = focal_summary, aes(x = chemical, y = mean, 
                                         ymax = mean + std_error, 
                                         ymin = mean - std_error), width = 0) +
    # Aesthetic customization
    labs(x = "Chemical", y = "Significant Changes in Concentration (Mean Response ± SE)") +
    scale_color_manual(values = chem_palt) +
    scale_fill_manual(values = chem_palt) +
    theme_facetbox +
    theme(strip.text.y = element_text(size = 11))
  
  # For all but left-most chemical, remove y-axis title
  if(focal_chem != "DIN"){ focal_panel <- focal_panel + 
    theme(axis.title.y = element_blank()) }
  
  # For all but right-most chemical, remove strip text on y-axis
  if(focal_chem != "DIP"){focal_panel <- focal_panel + 
    theme(strip.text.y = element_blank()) }
  
  # Add to figure list
  panel_list[[focal_chem]] <- focal_panel
  
} # Close list

# Assemble figure
cowplot::plot_grid(plotlist = panel_list, ncol = 5)

# Export locally
ggsave(filename = file.path("graphs", "figures", "fig_mean-response-by-chem-and-lter.png"),
       height = 12, width = 8, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Conc. Violins by LTER & Chemical (% Change) ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Read in necessary data file(s)
df_conc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                               pattern = "_Conc_uM_"),
                      .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  # Stack them vertically
  purrr::list_rbind(x = .) %>% 
  # Drop Canada (lacks many chemicals)
  dplyr::filter(!LTER %in% c("Canada")) %>% 
  # Keep only significant slopes - excluding marginal
  # dplyr::filter(significance %in% c("sig")) %>% # excluding for now
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>% 
  # Pare down to only desired columns
  dplyr::select(sizer_groups, LTER, Stream_Name, chemical, mean_response, percent_change) %>% 
  # Standardize names of LTERs / chemicals
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Sweden", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>%
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>%
  dplyr::mutate(chemical = gsub(pattern = "_", replacement = ":", x = chemical)) %>% 
  # Order chemicals
  dplyr::mutate(chemical = factor(chemical, levels = c("DIN", "Si:DIN", "DSi", 
                                                       "Si:DIP", "DIP")))

# Check structure
dplyr::glimpse(df_conc)

# Summarize as well for mean +/- SE bars
df_summary <- supportR::summary_table(data = df_conc, response = "percent_change",
                                      groups = c("LTER", "chemical"))

# Check that out
dplyr::glimpse(df_summary)

# Generate desired graph
ggplot(df_conc, aes(x = chemical, y = percent_change)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_jitter(aes(color = chemical), width = 0.15, alpha = 0.25) +
  geom_violin(aes(fill = chemical), alpha = 0.2) +
  # Facet by LTER & chemical
  facet_grid(LTER ~ ., scales = "free_y") +
  # Add averaged points with SE bars
  geom_point(data = df_summary, aes(x = chemical, y = mean, fill = chemical), 
             size = 3, shape = 21) +
  geom_errorbar(data = df_summary, aes(x = chemical, y = mean, 
                                          ymax = mean + std_error, 
                                          ymin = mean - std_error), width = 0) +
  # Aesthetic customization
  labs(x = "Chemical", y = "Concentration % Change (Mean ± SE)") +
  scale_color_manual(values = chem_palt) +
  scale_fill_manual(values = chem_palt) +
  theme_facetbox +
  theme(strip.text.y = element_text(size = 11))

# Export locally
ggsave(filename = file.path("graphs", "figures", "fig_perc-change-by-chem-and-lter.png"),
       height = 12, width = 8, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# FN vs. Actual Concentration ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Read in necessary data file(s)
df_conc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                                   pattern = "_Conc_uM_"),
                          .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  # Stack them vertically
  purrr::list_rbind(x = .) %>% 
  # Add column for normalization
  dplyr::mutate(normalize = "Not")

# Do same for flow-normalized equivalents
df_fnconc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                                 pattern = "_Conc_uM_"),
                        .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  purrr::list_rbind(x = .) %>% 
  dplyr::mutate(normalize = "FN")

# Combine these two
df_combo <- dplyr::bind_rows(df_conc, df_fnconc) %>% 
  # Drop Canada (lacks many chemicals)
  dplyr::filter(!LTER %in% c("Canada")) %>% 
  # Keep only significant slopes - excluding marginal
  # dplyr::filter(significance %in% c("sig")) %>% # excluding for now
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>% 
  # Pare down to only desired columns
  dplyr::select(normalize, sizer_groups, LTER, Stream_Name, chemical, mean_response, percent_change) %>% 
  # Standardize names of LTERs / chemicals
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Sweden", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>%
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>%
  dplyr::mutate(chemical = gsub(pattern = "_", replacement = ":", x = chemical)) %>% 
  # Order chemicals
  dplyr::mutate(chemical = factor(chemical, levels = c("DIN", "Si:DIN", "DSi", "Si:DIP", "DIP"))) %>% 
  # Make normalization method + chemical column
  dplyr::mutate(norm_chem = paste0(normalize, "_", chemical), .after = normalize)

# Check structure
dplyr::glimpse(df_combo)

# Summarize as well for mean +/- SE bars
df_summary <- supportR::summary_table(data = df_combo, response = "mean_response",
                                      groups = c("normalize", "LTER", "chemical")) %>% 
  # Make LTER + chemical column
  dplyr::mutate(norm_chem = paste0(normalize, "_", chemical), .after = normalize)

# Check that out
dplyr::glimpse(df_summary)

# Assemble graph panels
for(focal_chem in levels(df_combo$chemical)){
  # focal_chem <- "DIN"
  
  # Processing message
  message("Making graph panel for ", focal_chem)
  
  # Subset data
  focal_combo <- dplyr::filter(df_combo, chemical == focal_chem)
  focal_summary <- dplyr::filter(df_summary, chemical == focal_chem)

  # Generate graph
  ggplot(focal_combo, aes(x = normalize, y = mean_response)) +
    geom_jitter(aes(color = norm_chem), width = 0.15, alpha = 0.25) +
    geom_violin(aes(fill = norm_chem), alpha = 0.2) +
    # Facet by LTER & chemical
    facet_wrap(LTER ~ .) +
    # Add averaged points with SE bars
    geom_point(data = focal_summary, aes(x = normalize, y = mean, 
                                      fill = norm_chem, shape = normalize), 
               size = 3) +
    geom_errorbar(data = focal_summary, aes(x = normalize, y = mean, 
                                         ymax = mean + std_error, 
                                         ymin = mean - std_error), width = 0) +
    # Aesthetic customization
    labs(x = "Flow-Normalization Status", 
         y = paste("Sig. Changes in", focal_chem,  "Concentration (Mean ± SE)")) +
    scale_color_manual(values = normchem_palt) +
    scale_fill_manual(values = normchem_palt) +
    scale_shape_manual(values = c("FN" = 24, "Not" = 21)) +
    theme_facetbox +
    theme(strip.text.y = element_text(size = 11))
  
  # Make a local filename
  focal_file <- paste0("fig_conc-vs-fnconc-", tolower(focal_chem), ".png")
  
  # Export locally
  ggsave(filename = file.path("graphs", "figures", focal_file),
         height = 6, width = 8, units = "in")
  
}

# Tidy environment
rm(list = ls()); gc()

# End ----
