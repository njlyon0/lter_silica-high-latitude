## ------------------------------------------------------- ##
          # `SiZer` Workflow for Silica Export WG
## ------------------------------------------------------- ##
# Written by: Nick J Lyon & Joanna Carey

## ----------------------------------------- ##
              # Housekeeping ----
## ----------------------------------------- ##

# Need to force an install of HERON to get an updated version?
# devtools::install_github("lter/HERON", force = T)

# Load libraries
# install.packages("librarian")
librarian::shelf(broom, cowplot, googledrive, SiZer, tidyverse, lter/HERON, supportR)

# Clear environment
rm(list = ls())

# Identify files in Drive folder
ids <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1V5EqmOlWA8U9NWfiBcWdqEH9aRAP-zCk")) %>%
  # Filter to desired data files
  dplyr::filter(name %in% c("Full_Results_ResultsTable_GFN_WRTDS.csv"))

# Check that includes all desired data files
ids

# Create a folder to save to
dir.create(path = "data", showWarnings = F)

# Download desired data
purrr::walk2(.x = ids$id, .y = ids$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path("data", .y),
                                                overwrite = T))

# Load data
data_v0 <- readr::read_csv(file = file.path("data", "Full_Results_ResultsTable_GFN_WRTDS.csv"))

# Now subset to sites of interest
data_simp <- data_v0 %>%
  # Keep only polar sites
  dplyr::filter(LTER %in% c("MCM", "ARC", "GRO", "Finnish Environmental Institute","NIVA") | stream %in% c("Site 7")) %>%
  # But drop one site that is technically polar
  dplyr::filter(!stream %in% c("Site 69038", "Kymijoki Ahvenkoski 001", 
                               "Kymijoki Kokonkoski 014")) %>%
  # Convert 10^-6 xx to just xx
  dplyr::mutate(Flux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                    yes = (Flux_10_6kg_yr * 10^6),
                                    no = NA),
                FNFlux_kg_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (FNFlux_10_6kg_yr * 10^6),
                                      no = NA),
                Flux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                      yes = (Flux_10_6kmol_yr * 10^6),
                                      no = NA),
                FNFlux_kmol_yr = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                        yes = (FNFlux_10_6kmol_yr * 10^6),
                                        no = NA),
                Yield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                           yes = (Yield_10_6kmol_yr_km2 * 10^6),
                                           no = NA),
                FNYield_kmol_yr_km2 = ifelse(test = !chemical %in% c("Si:P", "Si:DIN"),
                                             yes = (FNYield_10_6kmol_yr_km2 * 10^6),
                                             no = NA)) %>%
  # Tweak chemical names to exclude `:` in ratio
  dplyr::mutate(chemical = dplyr::case_when(
    chemical == "Si:DIN" ~ "Si_DIN",
    chemical == "Si:P" ~ "Si_P",
    TRUE ~ chemical))
  
# Take a look!
dplyr::glimpse(data_simp)

# Check lost/gained columns
supportR::diff_check(old = names(data_v0), new = names(data_simp))

# Clean up environment
rm(list = setdiff(ls(), c("data_simp")))

## ----------------------------------------- ##
          # Pre-Loop Preparation ----
## ----------------------------------------- ##

# Identify response (Y) and explanatory (X) variables
response_var <- "Yield_kmol_yr_km2"
## Yield, Conc_uM, Discharge_cms
explanatory_var <- "Year"

# Identify which chemical you want to analyze
element <- "DSi"
## DSi, Si_DIN, Si_P

# Do a quick typo check
if(!response_var %in% names(data_simp)) {
  message("Response not found in data! Check spelling.") } else {
    message("Response variable looks good!") }
if(!explanatory_var %in% names(data_simp)) {
  message("Explanatory not found in data! Check spelling.") } else {
    message("Explanatory variable looks good!") }
if(!element %in% data_simp$chemical) {
  message("Chemical not found in data! Check spelling.") } else {
    message("Chemical looks good!") }

# Identify the bandwidth to use with SiZer
bandwidth <- 5

# Carve data down to only needed bits
data_short <- data_simp %>%
  ## Keep only needed columns
  dplyr::select(LTER:chemical, dplyr::starts_with(response_var)) %>%
  ## And only chemical of interest
  dplyr::filter(chemical == element)

# Check that out
dplyr::glimpse(data_short)

# Make sure there are some non-NAs for the selected response/explanatory/chemical
if(all(is.na(data_short[[response_var]])) == T){
  message("No non-NA response values found in this subset of the data")
} else { message("Data subset looks good!") }

# Create a folder to save experimental outputs
# Folder name is: [response]_bw[bandwidths]_[date]
(export_folder <- paste0("export_", response_var, "_",
                         element, "_bw", bandwidth,
                         "_", Sys.Date()))
dir.create(path = export_folder, showWarnings = FALSE)

# Make an empty list to store all of our extracted information
giant_list <- list()

# Make a counter and set it to 1 (the list will add to it)
j <- 1

## ----------------------------------------- ##
      # Loop Extract of SiZer Data ----
## ----------------------------------------- ##

# Loop through sites and extract information
for(place in unique(data_short$stream)) {
  # for(place in c("Yukon", "Site 7")) {
  
  # Start with a message!
  message("Processing begun for '", response_var, "' of '", element, "' at '", place, "'")
  
  # Subset the data
  data_sub <- data_short %>%
    dplyr::filter(stream == place) %>%
    as.data.frame()
  
  # Loop - Get SiZer Object ----
  message("Run SiZer...")
  
  # Invoke the SiZer::SiZer function
  e <- SiZer::SiZer(x = data_sub[[explanatory_var]],
                    y = data_sub[[response_var]],
                    h = c(2, 10), degree = 1,
                    derv = 1, grid.length = 100)
  
  # Make a shorter place name
  place_short <- stringr::str_sub(string = place, start = 1, end = 8)
  
  # Plot (and export) the SiZer object with horizontal lines of interest
  png(filename = file.path(export_folder, paste0(place_short, "_SiZer-plot.png")),
      width = 5, height = 5, res = 720, units = 'in')
  HERON::sizer_plot(sizer_object = e,
                    bandwidth_vec = c(bandwidth))
  dev.off()
  
  # Identify inflection points/slope changes
  sizer_info <- HERON::sizer_slice(sizer_object = e, bandwidth = bandwidth)
  
  # Loop - No Changes Workflow ----
  ## If no slope changes are found:
  if(nrow(sizer_info) == 0){
    
    # Message this status
    message("No slope changes/inflections found; Proceeding...")
    
    # Migrate "groups" over 
    data_info <- HERON::id_slope_changes(raw_data = data_sub,
                                         sizer_data = sizer_info,
                                         x = explanatory_var,
                                         y = response_var,
                                         group_dig = 5)
    
    # Make plot
    demo_plot <- HERON::sizer_ggplot(raw_data = data_info,
                                     sizer_data = sizer_info,
                                     x = explanatory_var, y = response_var,
                                     trendline = 'sharp', vline = "none") +
      ggtitle(label = paste0("h = ", bandwidth, " Slope Changes (None)"))
    
  } else {
    
    # If there are changes and/or inflections, find inflections
    inflects_raw <- c(sizer_info$neg_to_pos, sizer_info$pos_to_neg)
    inflects <- inflects_raw[!is.na(inflects_raw)]
    
    # Loop - Inflection Point Workflow ----
    # If any inflections *are* found:
    if(length(inflects > 0)){
      
      # Message to this effect
      message("Inflections found; Proceeding...")
      
      # Migrate groups over
      data_info <- HERON::id_inflections(raw_data = data_sub,
                                         sizer_data = sizer_info,
                                         x = explanatory_var,
                                         y = response_var,
                                         group_dig = 5)
      
      # Make plot
      demo_plot <- HERON::sizer_ggplot(raw_data = data_info,
                                       sizer_data = sizer_info,
                                       x = explanatory_var, y = response_var,
                                       trendline = 'sharp', vline = "inflections",
                                       sharp_colors = c("#bbbbbb", "green")) +
        ggtitle(label = paste0("h = ", bandwidth, " Inflection Points"))
      
    } else {
      # Loop - Slope Change Workflow ----
      
      # Message
      message("Slope changes found but no inflections; Proceeding...")
      
      # Strip group assignments
      data_info <- HERON::id_slope_changes(raw_data = data_sub, sizer_data = sizer_info,
                                           x = explanatory_var, y = response_var,
                                           group_dig = 5)
      
      # Plot 
      demo_plot <- HERON::sizer_ggplot(raw_data = data_info,
                                       sizer_data = sizer_info,
                                       x = explanatory_var, y = response_var,
                                       trendline = 'sharp', vline = "changes",
                                       sharp_colors = c("#bbbbbb", "green")) +
        ggtitle(label = paste0("h = ", bandwidth, " Slope Changes"))
    } } # Close tri-partite workflow splits
  
  # Export whichever graph got made
  ggplot2::ggsave(filename = file.path(export_folder, 
                                       paste0(place_short, "_ggplot.png")),
                  height = 8, width = 8)
  
  # Loop - Wrangle SiZer Data ----
  message("Wrangling SiZer data...")
  
  # Modify the columns in the provided sizer dataframes
  sizer_export <- data_info %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(bandwidth_h = bandwidth,
                  site = place, 
                  .before = dplyr::everything()) %>%
    as.data.frame()
  
  # Add this tidied dataframe to our export list
  giant_list[[paste0("data_", j)]] <- sizer_export
  
  # Loop - Fit Linear Models ----
  message("Fit regressions...")
  
  # Extract statistics/estimates from linear models
  lm_obj <- HERON::sizer_lm(data = data_info, x = explanatory_var,
                            y = response_var, group_col = "groups") %>%
    # Then add column for bandwidth
    purrr::map(.f = mutate, bandwidth_h = bandwidth,
               .before = dplyr::everything())
  
  # Final dataframe processing for *statistics*
  stat_df <- lm_obj[[1]] %>%
    # Make all columns characters
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    # Add a site column
    dplyr::mutate(site = place, .before = dplyr::everything())
  
  # Final dataframe processing for *estimates*
  est_df <- lm_obj[[2]] %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(site = place, .before = dplyr::everything())
  
  # Add this information to their respective lists
  giant_list[[paste0("stats_", j)]] <- stat_df
  giant_list[[paste0("estimates_", j)]] <- est_df
  
  # Increase the counter by 1 (for the next iteration of the loop)
  j <- j + 1
  
  # Return a "finished" message!
  message("Processing complete for '", response_var, "' of '", element, "' at '", place, "'")
  
} # Close loop

## ----------------------------------------- ##
          # Process Loop Outputs ----
## ----------------------------------------- ##

# Check out what is in our huge list
names(giant_list)

# Make a new list to store simplified outputs in
result_list <- list()

# Now (ironically) we'll use a loop to unlist what the first loop made
for(data_type in c("data", "stats", "estimates")){
  
  # For each data type...
  list_sub <- giant_list %>%
    # Identify all list elements that contain this type of data
    purrr::keep(.p = stringr::str_detect(string = names(.),
                                         pattern = data_type)) %>%
    # Unlist by selecting all columns of each list element
    purrr::list_rbind()
  
  # Export each type of output in its 'raw' form for posterity
  write_csv(x = list_sub, na = "",
            file = file.path(export_folder,
                             paste0("_slope-change_", data_type, "_exported.csv")))
  
  # Add this to the simpler results list
  result_list[[data_type]] <- list_sub
  
  # And print a message
  message("Dataframe for ", data_type, " exported.") }

# Check out the simplified results list we're left with
names(result_list)
dplyr::glimpse(result_list)

# Clear environment except for that result list
rm(list = setdiff(ls(), c("result_list")))

## ----------------------------------------- ##
        # Create Summary Outputs ----
## ----------------------------------------- ##

# Grab each bit as a dataframe for ease of further modification
estimates <- result_list[["estimates"]]
stats <- result_list[["stats"]]
years <- result_list[["data"]]

# Wrangle estimate part
est_v2 <- estimates %>%
  # Remove intercept information
  dplyr::filter(term != "(Intercept)") %>%
  # Drop term column now that it's all "data[[x]]"
  dplyr::select(-term) %>%
  # Rename columns with periods in names to use underscores
  dplyr::rename(std_error = std.error,
                p_value = p.value)

# Check structure
dplyr::glimpse(est_v2)

# Wrangle statistics part
stats_v2 <- stats %>%
  # Pare down to only desired columns (implicitly removes non-specified columns)
  dplyr::select(site, bandwidth_h, section, r.squared, adj.r.squared, AIC, BIC) %>%
  # Rename period columns to use underscores
  dplyr::rename(r_squared = r.squared,
                adj_r_squared = adj.r.squared)

# Check structure
dplyr::glimpse(stats_v2)

# Merge estimates and statistics data
combo_v1 <- dplyr::left_join(x = est_v2, y = stats_v2, by = c("site", "bandwidth_h", "section"))

# Check structure
dplyr::glimpse(combo_v1)

# Wrangle source data ('years')
years_v2 <- years %>%
  # Tweak the '-Inf to Inf' entry for consistency with other dataframes and rename column
  dplyr::mutate(section = ifelse(groups == "(-Inf, Inf]",
                                 yes = "No inflection points", no = groups), 
                .after = LTER) %>%
  # Pare down to only some columns
  dplyr::select(site, LTER, section, start, end, slope_type, chemical) %>%
  # Make end/start actually numbers and calculate duration of group
  dplyr::mutate(start = as.numeric(start),
                end = as.numeric(end),
                duration = end - start,
                .after = end)

# Check structure
dplyr::glimpse(years_v2)

# Attach that modified years object to the other combined dataframe
combo_v2 <- dplyr::left_join(combo_v1, years_v2, by = c("site", "section"))

# Check structure
dplyr::glimpse(combo_v2)

# Now we can wrangle all three types of data in a single object
combo_v3 <- combo_v2 %>%
  # Move grouping columns over to the left
  dplyr::relocate(LTER, .before = site) %>%
  dplyr::relocate(chemical, section, start, end, duration, slope_type, .after = site) %>%
  # Make several columns actually be numeric
  dplyr::mutate( dplyr::across(.cols = bandwidth_h:BIC, .fns = as.numeric)) %>%
  # Keep only p values that are significant
  dplyr::filter(p_value < 0.05) %>%
  # And only R squareds that are 'pretty good'
  dplyr::filter(r_squared >= 0.30) %>%
  # Arrange by LTER and site
  arrange(LTER, site)

# Double check structure
dplyr::glimpse(combo_v3)


# BASEMENT ----
## "basement" = storage area for code that is related to but not integral to the workflow preceding it


#filter for only significant slopes. remove pipe to see how many sign, then add in to see if lose any w/ r.square threshold

#lost two sign regressions for DSi yield and discharge and P conc, DIN yield
#lost 1 sign regression Si_DIN conc ratio (Oxyx), DIP yield
#lose zero sign regressions for DSi conc, Si_DIN ratios of fluxes and , Si_P ratios fluxes and conc, DIN conc

write.csv(Data4, file="SignificantSlopeswStats.csv", row.names=FALSE)

##plotting results
#this new column allows one to have boxplots sorted by lter, then stream
Data4$LTER_Site<-paste0(stringr::str_sub(Data4$LTER,1,3),"_",Data4$site) 

Data4<-Data4 %>%
  mutate(across (8:11, round, 2))

#changing the R2 text sizing and position for each output file - no room for it for Conc
#yield geom_text: hjust = -.25, vjust= 1.5, size=3.5
#need to change size of saved file too - height of conc = 8, yield = 6
ggplot(Data4) +
  geom_col(aes(x=estimate, y=LTER_Site, fill=duration)) +
  geom_errorbar(aes(xmin=estimate - std.error, xmax=estimate + std.error, y=LTER_Site), width=0.2, size=0.75, color="gray")+
  #geom_text(aes(label= r.squared, estimate, y= LTER_Site), hjust = -.25, vjust= 1.5, size=3.5) +
  theme_bw()+
  #geom_text(aes(label= start, estimate, y= LTERSite), hjust = -2, vjust= 2)
  ggtitle("Significant changes in DIN yield")
ggsave(paste0("Barchart SiZer Significnat change DIN conc",Sys.Date(),".png"), width=6, height=8)





# End ----
