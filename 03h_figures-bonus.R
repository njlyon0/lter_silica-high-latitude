## ------------------------------------------------------- ##
# Figure Creation
## ------------------------------------------------------- ##
# Purpose:
## Make publication-quality figures
## That are not currently part of the main text of the paper or in the supplemental info

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

## ----------------------------------------- ##
# Housekeeping ----
## ----------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cowplot, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "figures_bonus"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Chemical Monthly Bookmarks ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Identify conserved file stem
file_stem <- "stats-ready_monthly"

# Read in all chemical for this response
df_chem_all <- purrr::map(.x = dir(path = file.path("data", file_stem),
                                   pattern = "_Conc_uM"),
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
  dplyr::filter(stringr::str_detect(string = chemical, pattern = ":") != T) %>% 
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical, Year, Month,
                Conc_uM, significance, slope_direction, duration_bin) %>%
  dplyr::distinct()

# Count streams / LTER
(streams_per_lter <- lter_ct(data = df_chem_simp))

# Loop across chemicals
for(chem in unique(df_chem_simp$chemical)){
  # chem <- "DSi"
  
  # Message
  message("Creating bookmark graph for ", chem, " Concentration (uM)")
  
  # Subset data
  df_chem_sub <- dplyr::filter(df_chem_simp, chemical == chem)
  
  # Identify any streams that don't have data for this chemical
  df_chem_missing <- df_chem_all %>% 
    dplyr::filter(!LTER_stream %in% unique(df_chem_sub$LTER_stream)) %>% 
    dplyr::mutate(significance = "NA", slope_direction = "NA")
  
  # Re-attach any streams that were dropped (we want the same number of 'rows' in all graphs)
  df_chem <- dplyr::bind_rows(df_chem_sub, df_chem_missing)
  
  # Make an empty list for storing outputs
  mo_list <- list()
  
  # Loop across months
  for(mo in sort(unique(df_chem$Month))){
    # mo <- 1
    
    # Progress message
    message("Creating bookmark graph for month ", mo)
    
    # Subset data again
    df_chem_mo <- dplyr::filter(df_chem, Month == mo)
    
    # Identify any streams that don't have data for this month
    df_mo_missing <- df_chem_all %>% 
      dplyr::filter(!LTER_stream %in% unique(df_chem_mo$LTER_stream)) %>% 
      dplyr::mutate(significance = "NA", slope_direction = "NA")
    
    # Re-attach those streams
    df_chem_mo_actual <- dplyr::bind_rows(df_chem_mo, df_mo_missing)
    
    # Create the bookmark graph 
    q <- ggplot(data = df_chem_mo_actual, mapping = aes(x = Year, y = LTER_stream)) +
      # Add points with underlying lines for each section
      geom_path(aes(group = sizer_groups, color = slope_direction), 
                lwd = 2.5, alpha = 0.6) +
      geom_point(aes(group = sizer_groups, fill = slope_direction, 
                     shape = slope_direction), size = 2) +
      geom_point(data = df_chem_mo[df_chem_mo$slope_direction != "NA", ],
                 aes(shape = slope_direction), color = "white", size = 2, fill = NA) +
      # Manually specify point/line colors and point shapes
      scale_color_manual(values = dir_palt, breaks = c("pos", "neg", "NS", "NA"), 
                         guide = "none") +
      scale_fill_manual(values = dir_palt, breaks = c("pos", "neg", "NS", "NA")) +
      scale_shape_manual(values = dir_shps, breaks = c("pos", "neg", "NS", "NA")) +
      # Add lines between streams from different LTERs
      geom_hline(yintercept = streams_per_lter$line_positions) +
      # Customize labels and axis titles
      labs(x = "Year", y = "Stream", title = mo) +
      # Modify theme elements for preferred aesthetics
      theme_bookmark +
      theme(legend.position = "inside",
            legend.position.inside = c(0.3, 0.88),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_blank())
    
    # Remove the legend from all months except January
    if(mo > 1){
      q <- q +
        theme(legend.position = "none")
    }
    
    # Add research network annotations to only January
    if(mo %in% c(1, 7)){
      q <- q +
        geom_text(x = 1989, y = 1.5, label = "Canada", color = "black", hjust = "left") +
        annotate(geom = "text", x = 1990, color = "black", angle = 90, hjust = "center",
                 y = c(14, 27.5, 35.5, 44, 50.5, 62),
                 label = c("Finland", "GRO", "Krycklan", "MCM", "Norway", "Sweden"))
    }
    
    # Add to list
    mo_list[[paste0("month_", mo)]] <- q
    
  } # Close month loop
  
  # Assemble the desired figure
  cowplot::plot_grid(plotlist = mo_list, nrow = 2, ncol = 6)
  
  # And export it
  ggsave(filename = file.path("graphs", "figures_bonus", 
                              paste0("fig_bookmark_monthly-", tolower(chem), "_conc_um.png")),
         height = 16, width = 20, units = "in")
  
} # Close chem loop

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
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 16))
  
  # Generate local file name
  focal_out <- paste0("fig_boxplot-chemicals-", tolower(focal_chem), "-zoom_conc_um.png")
  
  # Export locally
  ggsave(filename = file.path("graphs", "figures_bonus", focal_out),
         height = 2, width = 6, units = "in")
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
  dplyr::mutate(chemical = ifelse(stringr::str_detect(string = chemical, pattern = ":"),
                                  yes = gsub("Si", "DSi", chemical),
                                  no = chemical)) %>% 
  dplyr::mutate(chemical = factor(chemical, levels = c("DIN", "DSi:DIN", "DSi", 
                                                       "DSi:DIP", "DIP")))

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
    theme(strip.text.y = element_text(size = 12))
  
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
ggsave(filename = file.path("graphs", "figures_bonus", "fig_mean-response-by-chem-and-lter.png"),
       height = 12, width = 8, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# FN vs. Actual Conc. (Mean Resp / LTER) ----
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
  dplyr::mutate(chemical = ifelse(stringr::str_detect(string = chemical, pattern = ":"),
                                  yes = gsub("Si", "DSi", chemical),
                                  no = chemical)) %>% 
  dplyr::mutate(chemical = factor(chemical, levels = c("DIN", "DSi:DIN", "DSi", 
                                                       "DSi:DIP", "DIP"))) %>% 
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
    theme(strip.text.y = element_text(size = 12))
  
  # Make a local filename
  focal_file <- paste0("fig_conc-vs-fnconc-", tolower(focal_chem), ".png")
  
  # Export locally
  ggsave(filename = file.path("graphs", "figures_bonus", focal_file),
         height = 6, width = 8, units = "in")
  
}

# Tidy environment
rm(list = ls()); gc()

# End ----
