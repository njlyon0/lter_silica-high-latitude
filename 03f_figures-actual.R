## ------------------------------------------------------- ##
# Figure Creation - Actual
## ------------------------------------------------------- ##
# Purpose:
## Make publication-quality figures
## THAT ARE ACTUALLY INCLUDED IN THE MAIN TEXT OF THE PAPER

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cowplot, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "figures_actual"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Figure 1 - Site Map ----
## ----------------------------------------- ##

# This is the site map, see `03c_site-map.R`

## ----------------------------------------- ##
# Figure 2 - Non-FN *Conc* Chemical Bookmarks ----
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

# Read in all chemical for this response
df_chem_all <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                                   pattern = "_Conc_uM_"),
                          .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
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
                  Conc_uM, significance, slope_direction, duration_bin) %>%
    dplyr::distinct() %>% 
    # Add "D" to chemical ratios
    dplyr::mutate(chemical = ifelse(stringr::str_detect(string = chemical, pattern = ":"),
                                    yes = gsub("Si", "DSi", chemical),
                                    no = chemical))
  
  # Count streams / LTER
  (streams_per_lter <- lter_ct(data = df_chem_simp))
  
  # Make a list of the desired number of elements
  chem_bookmarks <- list()
  
  # Loop across chemicals
  for(chem in unique(df_chem_simp$chemical)){
    # chem <- "DSi"
    
    # Message
    message("Creating bookmark graph for ", chem, " (Concentration uM)")
    
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
               label = c("Finland", "GRO", "Krycklan", "MCM", "Nor", "Sweden")) +
      # Customize labels and axis titles
      labs(x = "Year", y = "Stream", 
           title = paste0(chem, " Concentration (uM)")) +
      # Modify theme elements for preferred aesthetics
      theme_bookmark +
      theme(legend.position = "inside",
            legend.position.inside = c(0.225, 0.88))
    
    # Remove the legend from all but specified chemicals
    if(!chem %in% c("DSi", "DSi:DIN")){
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
  ggsave(filename = file.path("graphs", "figures_actual", 
    "fig-02A_bookmark-chemicals_conc-um.png"),
    height = 9, width = 15, units = "in")
  
  # Assemble & export the second figure (ratios only)
  cowplot::plot_grid(chem_bookmarks[["DSi:DIN"]], chem_bookmarks[["DSi:DIP"]], nrow = 1)
  ggsave(filename = file.path("graphs", "figures_actual", 
    "fig-02B_bookmark-chemicals_conc-um.png"),
    height = 9, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Figure 3 - FN vs Conc. Violins by LTER & Chemical (% Change) ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Read in necessary data file(s)
df_conc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                               pattern = "_Conc_uM_|_FNConc_uM"),
                      .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  # Stack them vertically
  purrr::list_rbind(x = .) %>% 
  # Add a column for normalization
  dplyr::mutate(normalize = ifelse(is.na(FNConc_uM) != T,
                                   yes = "FN", no = "Not")) %>% 
  # Drop Canada (lacks many chemicals)
  dplyr::filter(!LTER %in% c("Canada")) %>% 
  # Keep only significant slopes - excluding marginal
  # dplyr::filter(significance %in% c("sig")) %>% # excluding for now
  # Keep only certain durations of trends
  dplyr::filter(section_duration >= 5) %>% 
  # Pare down to only desired columns
  dplyr::select(sizer_groups, LTER, Stream_Name, normalize, chemical, 
                mean_response, percent_change) %>% 
  dplyr::distinct() %>% 
  # Standardize names of LTERs / chemicals
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Sweden", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>%
  dplyr::mutate(chemical = gsub(pattern = "P", replacement = "DIP", x = chemical)) %>%
  dplyr::mutate(chemical = gsub(pattern = "_", replacement = ":", x = chemical)) %>%
  # Order chemicals
  dplyr::mutate(chemical = ifelse(stringr::str_detect(string = chemical, pattern = ":"),
                                  yes = gsub("Si", "DSi", chemical),
                                  no = chemical)) %>% 
  dplyr::mutate(chemical = factor(chemical, levels = c("DIN", "DSi:DIN", "DSi", 
                                                       "DSi:DIP", "DIP"))) %>% 
  # Generate 'norm chem' combo column
  dplyr::mutate(norm_chem = paste0(normalize, "_", chemical), .after = normalize)

# Check structure
dplyr::glimpse(df_conc)

# Summarize as well for mean +/- SE bars
df_summary <- supportR::summary_table(data = df_conc, response = "percent_change",
                                      groups = c("LTER", "normalize", "norm_chem", "chemical"))

# Check that out
dplyr::glimpse(df_summary)

# Generate desired graph
ggplot(df_conc, aes(x = normalize, y = percent_change)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_jitter(aes(color = norm_chem), width = 0.15, alpha = 0.25) +
  geom_violin(aes(fill = norm_chem), alpha = 0.2) +
  # Facet by LTER & chemical
  facet_grid(LTER ~ chemical, scales = "free_y") +
  # Add averaged points with SE bars
  geom_point(data = df_summary, aes(x = normalize, y = mean, fill = norm_chem), 
             size = 3, shape = 21) +
  geom_errorbar(data = df_summary, aes(x = normalize, y = mean, 
                                       ymax = mean + std_error, 
                                       ymin = mean - std_error), width = 0) +
  # Aesthetic customization
  labs(x = "Chemical", y = "Concentration % Change (Mean ± SE)") +
  scale_color_manual(values = normchem_palt) +
  scale_fill_manual(values = normchem_palt) +
  theme_facetbox +
  theme(strip.text.y = element_text(size = 12))

# Export locally
ggsave(filename = file.path("graphs", "figures_actual",
  "fig-03_conc-perc-change-by-chem-and-lter-and-normalize.png"),
  height = 12, width = 8, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Figure 4 - Non-FN *Yield* Chemical Bookmarks ----
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

# Read in all chemical for this response
df_chem_all <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                                   pattern = "_Yield_"),
                          .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
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
                  Yield, significance, slope_direction, duration_bin) %>%
    dplyr::distinct() %>% 
    # Add "D" to chemical ratios
    dplyr::mutate(chemical = ifelse(stringr::str_detect(string = chemical, pattern = ":"),
                                    yes = gsub("Si", "DSi", chemical),
                                    no = chemical))
  
  # Count streams / LTER
  (streams_per_lter <- lter_ct(data = df_chem_simp))
  
  # Make a list of the desired number of elements
  chem_bookmarks <- list()
  
  # Loop across chemicals
  for(chem in unique(df_chem_simp$chemical)){
    # chem <- "DSi"
    
    # Message
    message("Creating bookmark graph for ", chem, " (Yield)")
    
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
               label = c("Finland", "GRO", "Krycklan", "MCM", "Nor", "Sweden")) +
      # Customize labels and axis titles
      labs(x = "Year", y = "Stream", 
           title = paste0(chem, " Yield")) +
      # Modify theme elements for preferred aesthetics
      theme_bookmark +
      theme(legend.position = "inside",
            legend.position.inside = c(0.225, 0.88))
    
    # Remove the legend from all but specified chemicals
    if(!chem %in% c("DSi", "DSi:DIN")){
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
  ggsave(filename = file.path("graphs", "figures_actual", 
    "fig-04A_bookmark-chemicals_yield.png"),
    height = 9, width = 15, units = "in")
  
  # Assemble & export the second figure (ratios only)
  cowplot::plot_grid(chem_bookmarks[["DSi:DIN"]], chem_bookmarks[["DSi:DIP"]], nrow = 1)
  ggsave(filename = file.path("graphs", "figures_actual", 
    "fig-04B_bookmark-chemicals_yield.png"),
    height = 9, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Figure 5 - Stacked Barplots of DSi Conc & Discharge ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Read in relevant data
si <- read.csv(file = file.path("data", "stats-ready_monthly", "stats-ready_monthly_Conc_uM_DSi.csv")) %>% 
  # Remove McMurdo streams with incomplete chemical information
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream "))
# Check structure
dplyr::glimpse(si)

# Parse the data into the necessary format
si_v2 <- si %>% 
  # Standardize some LTER names
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Sweden", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>% 
  # Flesh out 'slope direction' column slightly
  dplyr::mutate(slope_direction = dplyr::case_when(
    significance == "NS" ~ "NS",
    significance == "marg" ~ "NS",
    is.na(slope_direction) == T ~ "NA",
    T ~ slope_direction),
    slope_direction = factor(slope_direction, levels = c("pos", "NS", "neg", "NA"))) %>% 
  # Get just necessary columns & unique rows
  dplyr::select(sizer_groups, LTER, Year, Month, LTER_stream, slope_direction) %>% 
  dplyr::distinct() %>% 
  # Calculate necessary summary info within groups
  dplyr::group_by(LTER, Month) %>% 
  dplyr::mutate(total_ct = dplyr::n()) %>% 
  dplyr::group_by(LTER, Month, total_ct, slope_direction) %>% 
  dplyr::summarize(slope_ct = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  # Define other key variables
  dplyr::mutate(prop_slope = slope_ct / total_ct)

# Re-check structure
dplyr::glimpse(si_v2)

# Create desired graph
stack_si <- ggplot(si_v2, aes(x = as.factor(Month), y = prop_slope, 
                  fill = slope_direction, color = "x")) +
  geom_bar(stat = "identity") +
  facet_wrap(LTER ~ ., ncol = 4, axes = "all_x") +
  scale_fill_manual(values = dir_palt) +
  scale_color_manual(values = "#000") +
  guides(color = "none") +
  labs(x = "Month", y = "Slope Direction Proportion") +
  theme_facetbox +
  theme(legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.justification = c(1.8, -0.5),
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# Check that out
stack_si

# Read in relevant data
disc <- read.csv(file = file.path("data", "stats-ready_monthly", "stats-ready_monthly_Discharge_cms_DSi.csv")) %>% 
  # Remove McMurdo streams with incomplete chemical information
  dplyr::filter(!LTER_stream %in% c("MCM_Commonwealth S", "MCM_Crescent Strea", 
                                    "MCM_Delta Stream  ", "MCM_Harnish Creek ",
                                    "MCM_Onyx River  La", "MCM_Onyx River  Lo",
                                    "MCM_Priscu Stream "))
# Check structure
dplyr::glimpse(disc)

# Parse the data into the necessary format
disc_v2 <- disc %>% 
  # Standardize some LTER names
  dplyr::mutate(LTER = gsub(pattern = "MCM", replacement = "McMurdo", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Finnish Environmental Institute", replacement = "Finland", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "Swedish Goverment", replacement = "Sweden", x = LTER)) %>%
  dplyr::mutate(LTER = gsub(pattern = "NIVA", replacement = "Norway", x = LTER)) %>% 
  # Flesh out 'slope direction' column slightly
  dplyr::mutate(slope_direction = dplyr::case_when(
    significance == "NS" ~ "NS",
    significance == "marg" ~ "NS",
    is.na(slope_direction) == T ~ "NA",
    T ~ slope_direction),
    slope_direction = factor(slope_direction, levels = c("pos", "NS", "neg", "NA"))) %>% 
  # Get just necessary columns & unique rows
  dplyr::select(sizer_groups, LTER, Year, Month, LTER_stream, slope_direction) %>% 
  dplyr::distinct() %>% 
  # Calculate necessary summary info within groups
  dplyr::group_by(LTER, Month) %>% 
  dplyr::mutate(total_ct = dplyr::n()) %>% 
  dplyr::group_by(LTER, Month, total_ct, slope_direction) %>% 
  dplyr::summarize(slope_ct = dplyr::n(),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Define other key variables
  dplyr::mutate(prop_slope = slope_ct / total_ct)

# Re-check structure
dplyr::glimpse(disc_v2)

# Create desired graph
stack_disc <- ggplot(disc_v2, aes(x = as.factor(Month), y = prop_slope, 
                    fill = slope_direction, color = "x")) +
  geom_bar(stat = "identity") +
  facet_wrap(LTER ~ ., ncol = 4, axes = "all_x") +
  scale_fill_manual(values = dir_palt) +
  scale_color_manual(values = "#000") +
  guides(color = "none") +
  labs(x = "Month", y = "Slope Direction Proportion") +
  theme_facetbox +
  theme(legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 8))

# Check that out
stack_disc

# Assemble into the final figure
cowplot::plot_grid(stack_si, stack_disc, ncol = 1, labels = "AUTO")

# Export locally
ggsave(filename = file.path("graphs", "figures_actual", 
  "fig-05_stack-bar_monthly-si-and-discharge.png"),
  height = 12, width = 8, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Figure 6 - Monthly *DSi Conc* (FN v. Not) Violins ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
  
# Read in necessary data file(s)
## Not normalized
month_v1 <- read.csv(file = file.path("data", "stats-ready_monthly", 
                                      "stats-ready_monthly_Conc_uM_DSi.csv")) %>% 
  dplyr::mutate(normalize = "Not")
## Flow-Normalized (FN)
fn_month_v1 <- read.csv(file = file.path("data", "stats-ready_monthly", 
                                          "stats-ready_monthly_FNConc_uM_DSi.csv")) %>% 
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

# Get a summary table for monthly data too
month_smry <- supportR::summary_table(data = month_v2, 
                                      groups = c("LTER", "norm_chem", "normalize", "Month"),
                                      response = "percent_change", drop_na = T)

# Make figure
ggplot() +
  geom_violin(month_v2, mapping = aes(x = normalize, y = percent_change, 
                                      fill = norm_chem), alpha = 0.8) +
  geom_point(month_smry, mapping = aes(x = normalize, y = mean, 
                                        fill = norm_chem), pch = 21, size = 3) +
  geom_errorbar(month_smry, mapping = aes(x = normalize, y = mean,
                                          ymax = mean + std_error,
                                          ymin = mean - std_error),
                width = 0.1) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(LTER ~ Month, scales = "free") +
  scale_fill_manual(values = normchem_palt) +
  labs(y = paste0("Significant DSi Concentration Change (%)"), 
        x = "Normalization", fill = "Normalize_Chem") +
  theme(panel.background = element_blank(),
        legend.position = "none",
        # legend.position.inside = c(0.6, leg_y),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing = unit(x = 2, units = "lines"))

# Export as a figure
ggsave(filename = file.path("graphs", "figures_actual", 
  "fig-06_monthly-boxplot-dsi-conc-vs-fnconc.png"),
  height = 12, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

#  End ----
