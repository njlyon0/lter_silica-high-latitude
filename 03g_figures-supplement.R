## ------------------------------------------------------- ##
# Figure Creation - Supplemental Info (SI)
## ------------------------------------------------------- ##
# Purpose:
## Make publication-quality figures
## THAT ARE INCLUDED IN THE SUPPLEMENTAL INFO OF THE PAPER

# Pre-Requisites:
## This script assumes you've run the "02_stats-prep.R" script

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, cowplot, supportR)

# Make a folder for exporting graphs
dir.create(path = file.path("graphs", "figures_supp-info"), showWarnings = F, recursive = T)

# Clear environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S1 - Conc vs. Lithology & Land Cover
## ----------------------------------------- ##

# For these graphs, see `03d_land-rock-graphs.R`

## ----------------------------------------- ##
# Fig S2 - Strip Boxplot Figure ----
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
ggsave(filename = file.path("graphs", "figures_supp-info", 
  "fig-supp02_strip-boxplot-chemicals_conc-um.png"),
  height = 7, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S3 - FN vs Yield Violins by LTER & Chemical (% Change) ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Read in necessary data file(s)
df_conc <- purrr::map(.x = dir(path = file.path("data", "stats-ready_annual"),
                               pattern = "_Yield_|_FNYield"),
                      .f = ~ read.csv(file = file.path("data", "stats-ready_annual", .x))) %>% 
  # Stack them vertically
  purrr::list_rbind(x = .) %>% 
  # Add a column for normalization
  dplyr::mutate(normalize = ifelse(is.na(FNYield) != T,
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
  labs(x = "Chemical", y = "Yield % Change (Mean ± SE)") +
  scale_color_manual(values = normchem_palt) +
  scale_fill_manual(values = normchem_palt) +
  theme_facetbox +
  theme(strip.text.y = element_text(size = 12))

# Export locally
ggsave(filename = file.path("graphs", "figures_supp-info",
  "fig-supp03_yield-perc-change-by-chem-and-lter-and-normalize.png"),
  height = 12, width = 8, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S4 - Discharge Bookmark Figure ----
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
ggsave(filename = file.path("graphs", "figures_supp-info",
  "fig-supp04_bookmark-discharge.png"),
  height = 9, width = 5, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S5 - Discharge Monthly Bookmarks ----
## ----------------------------------------- ##

# Load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_lter-ct.R"))

# Read in discharge data
df_q <- read.csv(file = file.path("data", "stats-ready_monthly", "stats-ready_monthly_Discharge_cms_DSi.csv")) %>% 
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
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, chemical, Year, Month,
                Discharge_cms, significance, slope_direction, duration_bin) %>%
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(df_q_simp)

# Count streams / LTER
(streams_per_lter <- lter_ct(data = df_q_simp))

# Make an empty list for storing outputs
mo_list <- list()

# Loop across months
for(mo in sort(unique(df_q_simp$Month))){
  # mo <- 1
  
  # Progress message
  message("Creating discharge bookmark graph for month ", mo)
  
  # Subset data again
  df_q_mo <- dplyr::filter(df_q_simp, Month == mo)
  
  # Identify any streams that don't have data for this month
  df_mo_missing <- df_q %>% 
    dplyr::filter(!LTER_stream %in% unique(df_q_mo$LTER_stream)) %>% 
    dplyr::mutate(significance = "NA", slope_direction = "NA")
  
  # Re-attach those streams
  df_q_mo_actual <- dplyr::bind_rows(df_q_mo, df_mo_missing)
  
  # Create the bookmark graph 
  q <- ggplot(data = df_q_mo_actual, mapping = aes(x = Year, y = LTER_stream)) +
    # Add points with underlying lines for each section
    geom_path(aes(group = sizer_groups, color = slope_direction), 
              lwd = 2.5, alpha = 0.6) +
    geom_point(aes(group = sizer_groups, fill = slope_direction, 
                   shape = slope_direction), size = 2) +
    geom_point(data = df_q_mo[df_q_mo$slope_direction != "NA", ],
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
ggsave(filename = file.path("graphs", "figures_supp-info",
  "fig-supp05_bookmark_monthly-discharge.png"),
  height = 16, width = 20, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S6 - *FN Conc* Chemical Bookmarks ----
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
                                   pattern = "_FNConc_uM_"),
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
                  FNConc_uM, significance, slope_direction, duration_bin) %>%
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
    message("Creating bookmark graph for ", chem, " (FN Concentration uM)")
    
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
           title = paste0(chem, " FN Concentration (uM)")) +
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
  ggsave(filename = file.path("graphs", "figures_supp-info", 
    "fig-supp06A_bookmark-chemicals_fn-conc-um.png"),
    height = 9, width = 15, units = "in")
  
  # Assemble & export the second figure (ratios only)
  cowplot::plot_grid(chem_bookmarks[["DSi:DIN"]], chem_bookmarks[["DSi:DIP"]], nrow = 1)
  ggsave(filename = file.path("graphs", "figures_supp-info", 
    "fig-supp06B_bookmark-chemicals_fn-conc-um.png"),
    height = 9, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S7 - *FN Yield* Chemical Bookmarks ----
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
                                   pattern = "_FNYield_"),
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
                  FNYield, significance, slope_direction, duration_bin) %>%
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
    message("Creating bookmark graph for ", chem, " (FN Yield)")
    
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
           title = paste0(chem, " FN Yield")) +
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
  ggsave(filename = file.path("graphs", "figures_supp-info", 
    "fig-supp07A_bookmark-chemicals_fn-yield.png"),
    height = 9, width = 15, units = "in")
  
  # Assemble & export the second figure (ratios only)
  cowplot::plot_grid(chem_bookmarks[["DSi:DIN"]], chem_bookmarks[["DSi:DIP"]], nrow = 1)
  ggsave(filename = file.path("graphs", "figures_supp-info", 
    "fig-supp07B_bookmark-chemicals_fn-yield.png"),
    height = 9, width = 10, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S8 - Monthly Concentration (FN v. Not) Violins ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))

# Loop across chemicals
for(focal_chem in c("DIN", "P")){
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
    labs(y = paste0("Significant ", pretty_chem, " Concentration Change (%)"), 
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
  ggsave(filename = file.path("graphs", "figures_supp-info", 
    paste0("fig-supp08_", tolower(pretty_chem), "-monthly-boxplot-conc-vs-fnconc.png")),
    height = 12, width = 10, units = "in")
}

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S9 - 'Pick Up Sticks' Mean DSi Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Read in DSi data
si_v1 <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))

# Process this as needed - remove MCM b/c wasn't used in MLR
si_v2 <- si_v1 %>% 
  # Create new column for water yield
  dplyr::mutate(Qnorm = mean_Discharge_cms / drainSqKm) %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical,
                mean_response, percent_change, Qnorm, land_total_forest, 
                dplyr::starts_with(c("slope_", "mean_"))) %>% 
  dplyr::select(-slope_estimate, -slope_direction, -slope_std_error,
                -dplyr::contains(c("_FNConc_", "_NO3_", "_DIN_", "_NH4_",
                                   "_NOx_", "_Si.DIN_", "_Si.P_"))) %>% 
  # Change certain column names to be more informative
  dplyr::rename(mean_si_conc = mean_response,
                perc.change_si_conc = percent_change) %>% 
  # remove MCM b/c wasn't used to create these stats
  dplyr::filter(!LTER %in% c("MCM")) %>%
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

#creating new dataframe without Krycklan for water yield graph
si_v3 <- si_v2 %>%
  dplyr::filter(!LTER %in% c("Krycklan"))

# Check structure
dplyr::glimpse(si_v2)
## tibble::view(si_v2)

## ET
avg_ET <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                      exp_var = "mean_evapotrans_kg.m2", sig = "ixn",
                      lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)",
       x = expression(paste("Mean Evapotranspiration (kg/", m^2, ")"))) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_ET

## Snow (Proportion Area)
avg_snow <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                        exp_var = "mean_snow_max.prop.area", sig = "ixn",
                        lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Snow (Max Proportion Area)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_snow

## Temperature
avg_temp <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                        exp_var = "mean_temp_degC", sig = "NS") +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean Temperature (C)") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.8),
        legend.direction = "vertical",
        axis.text = element_text(color = "black"),
        legend.background = element_blank()); avg_temp

## Phosphorus concentration
avg_pconc <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "mean_P_Conc_uM", sig = "ixn",
                         lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)",
       x = "Mean P Concentration (uM)") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_pconc

## Specific Discharge
avg_Qnorm <- stick_graph(data = si_v2, resp_var = "mean_si_conc",  
                         exp_var = "Qnorm", sig = "ixn",
                         lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)",
       x = expression(paste("Mean Water Yield (", m^3, "/s/k", m^2, ")"))) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); avg_Qnorm

## LTER boxplots
avg_box <- ggplot(si_v2, aes(x = LTER, y = mean_si_conc, fill = LTER)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = lter_palt) +
  scale_x_discrete(labels = c("Canada", "Finland", "GRO", "Krycklan", 
                              "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)", x = "LTER") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none"); avg_box

# Assemble into grid of plots
cowplot::plot_grid(avg_ET, avg_snow, avg_temp, avg_pconc, avg_Qnorm, avg_box, 
                   nrow = 2, labels = "AUTO")

# Export as a figure
ggsave(filename = file.path("graphs", "figures_supp-info",
  "fig-supp09_sticks_si-mean.png"),
  height = 10, width = 15, units = "in")

## Specific Discharge without Krycklan for insert
Qnorm2 <- stick_graph(data = si_v3, resp_var = "mean_si_conc",  
                      exp_var = "Qnorm", sig = "ixn",
                      lters = c("Canada", "Finland", "GRO", "Norway", "Sweden")) +
  labs(y = "Mean DSi Concentration (uM)",
       x = expression(paste("Mean Water Yield (", m^3, "/s/k", m^2, ")"))) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); Qnorm2

# Export as a figure
ggsave(filename = file.path("graphs", "figures_supp-info",
  "fig-supp09-inset_sticks_si-mean-qnorm_nokrycklan.png"),
  height = 3, width = 5, units = "in")

# Tidy environment
rm(list = ls()); gc()

## ----------------------------------------- ##
# Fig S10 - 'Pick Up Sticks' DSi % Change Figure ----
## ----------------------------------------- ##

# Re-load graph helpers & needed functions
source(file.path("tools", "flow_graph-helpers.R"))
source(file.path("tools", "fxn_stick-graph.R"))

# Read in DSi data
si_v1 <- read.csv(file = file.path("data", "stats-ready_annual", "stats-ready_annual_Conc_uM_DSi.csv"))

# Process this as needed
si_v2 <- si_v1 %>% 
  # Create new column for water yield
  dplyr::mutate(slope_Qnorm = slope_Discharge_cms / drainSqKm) %>% 
  # Pare down to only what is needed
  dplyr::select(sizer_groups, LTER, Stream_Name, LTER_stream, drainSqKm, chemical,
                mean_response, percent_change,
                dplyr::starts_with(c("slope_", "mean_"))) %>% 
  dplyr::select(-slope_estimate, -slope_direction, -slope_std_error,
                -dplyr::contains(c("_FNConc_", "_NO3_", "_NH4_",
                                   "_NOx_", "_Si.DIN_", "_Si.P_"))) %>% 
  # Change certain column names to be more informative
  dplyr::rename(mean_si_conc = mean_response,
                perc.change_si_conc = percent_change) %>% 
  # remove MCM b/c wasn't used to create these stats
  dplyr::filter(!LTER %in% c("MCM")) %>%
  # Drop non-unique rows (leftover from previously annual replication; now replicate is SiZer chunk)
  dplyr::distinct()

# Check structure
dplyr::glimpse(si_v2)
unique(si_v2$LTER)
## tibble::view(si_v2)

## Water Yield
perc_WaterYield <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                               exp_var = "slope_Qnorm", sig = "main") +
  labs(y = "DSi Concentration (% Change)",
       x = expression(paste("Water Yield (", m^3, "/s/k", m^2, ") Annual Change"))) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.direction = "vertical",
        axis.text = element_text(color = "black"),
        legend.background = element_blank()); perc_WaterYield

## Evaporation
perc_ET <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                       exp_var = "slope_evapotrans_kg.m2", sig = "ixn",
                       lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "DSi Concentration (% Change)",
       x = expression(paste("Evapotranspiration (kg/", m^2, ") Annual Change"))) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.75),
        legend.direction = "vertical",
        axis.text = element_text(color = "black"),
        legend.background = element_blank()); perc_ET

## Precipitation
perc_ppt <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_precip_mm.per.day", sig = "ixn",
                        lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "DSi Concentration (% Change)",
       x = "Precipitation (mm/day) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_ppt

## Snow (Proportion Area)
perc_snow <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                         exp_var = "slope_snow_max.prop.area", sig = "ixn",
                         lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "DSi Concentration (% Change)",
       x = "Snow (Max Proportion Area) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_snow

## Temperature
perc_temp <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                         exp_var = "slope_temp_degC", sig = "ixn",
                         lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
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

## Nitrogen concentration
perc_nconc <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                          exp_var = "slope_DIN_Conc_uM", sig = "ixn",
                          lters = c("Canada", "Finland", "GRO", "Krycklan", "Norway", "Sweden")) +
  labs(y = "DSi Concentration (% Change)",
       x = "N Concentration (uM) Annual Change") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_nconc

## NPP concentration
perc_npp <- stick_graph(data = si_v2, resp_var = "perc.change_si_conc",  
                        exp_var = "slope_npp_kgC.m2.year", sig = "NS") +
  labs(y = "DSi Concentration (% Change)",
       x = expression(paste("NPP Concentration (kg C/", m^2, "/yr) Annual Change"))) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")); perc_npp

## LTER boxplots
perc_box <- ggplot(si_v2, aes(x = LTER, y = perc.change_si_conc, fill = LTER)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = lter_palt, 
                    labels = c("Canada", "Finland", "GRO", 
                               "Krycklan", "Norway", "Sweden")) +
  scale_x_discrete(labels = c("Canada", "Finland", "GRO", "Krycklan", 
                              "Norway", "Sweden")) +
  labs(y = "DSi Concentration (% Change)", x = "LTER") +
  geom_text(label = "NS", x = 1, y = 14, hjust = "center") + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.75)); perc_box

# Assemble into grid of plots
cowplot::plot_grid(perc_ET, perc_ppt, perc_snow, perc_temp, perc_pconc, perc_nconc, perc_npp, perc_WaterYield,
                   perc_box, nrow = 3, labels = "AUTO")

# Export as a figure
ggsave(filename = file.path("graphs", "figures_supp-info",
  "fig-supp10_sticks_si_perc-change.png"),
  height = 10, width = 15, units = "in")

# Tidy environment
rm(list = ls()); gc()

#  End ----
