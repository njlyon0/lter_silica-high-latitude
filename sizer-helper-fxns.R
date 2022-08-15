## ----------------------------------------- ##
       # Custom `SiZer` Helper Functions
## ----------------------------------------- ##
# Written by: Nick J Lyon

# Read in needed libraries
librarian::shelf(SiZer, tidyverse)

# Function No. 1 - Extract and Average -----------------------------
## Purpose: Identifies all inflection points (i.e., changes in slope) across all bandwidths supplied to `SiZer::SiZer`. Averages within type of inflection point (i.e., +/- or -/+) to create 'aggregate' inflection points
sizer_aggregate <- function(sizer_object = NULL){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | class(sizer_object) != "SiZer")
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Strip SiZer object content into dataframe
  sizer_raw <- as.data.frame(sizer_object)
  
  # Perform necessary wrangling
  sizer_data <- sizer_raw %>%
    # Rename columns
    dplyr::rename(x_grid = x, h_grid = h, slope_becomes = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope_becomes != "insufficient data") %>%
    # Within bandwidth levels (h_grid)
    dplyr::group_by(h_grid) %>%
    # Identify whether the next value is the same or different
    dplyr::mutate(transition = dplyr::case_when(
      # First identify start of each group
      is.na(dplyr::lag(slope_becomes, n = 1)) ~ 'start',
      # Now identify whether each value is the same as or different than previous
      slope_becomes == dplyr::lag(slope_becomes, n = 1) ~ 'same',
      slope_becomes != dplyr::lag(slope_becomes, n = 1) ~ 'change'
    )) %>%
    # Filter to retain only those rows that indicate a slope change
    dplyr::filter(transition == "change") %>%
    # Lets also identify what type of change the transition was
    dplyr::mutate(change_type = dplyr::case_when(
      transition == "change" & slope_becomes == "increasing" ~ 'change_to_positive',
      transition == "change" & slope_becomes == "flat" ~ 'change_to_zero',
      transition == "change" & slope_becomes == "decreasing" ~ 'change_to_negative')) %>%
    # Account for if multiple of the same change happen in a curve
    dplyr::group_by(h_grid, change_type) %>%
    dplyr::mutate(change_count = seq_along(unique(x_grid))) %>%
    # Ungroup for subsequent operations
    dplyr::ungroup() %>%
    # Group by change type
    dplyr::group_by(change_count, change_type) %>%
    # And average the x_grid value
    dplyr::summarise(slope_becomes = dplyr::first(slope_becomes),
                     mean_x_v1 = mean(as.numeric(x_grid), na.rm = T),
                     sd_x_v1 = sd(as.numeric(x_grid), na.rm = T),
                     n_x_v1 = dplyr::n(),
                     se_x_v1 = sd_x_v1 / n_x_v1,
                     .groups = 'keep') %>%
    # Ungroup
    dplyr::ungroup() %>%
    # Sort from lowest to highest X
    dplyr::arrange(mean_x_v1) %>%
    # Handle the same "change" occurring twice
    ## Identify these cases
    dplyr::mutate(diagnostic = cumsum(ifelse(slope_becomes != dplyr::lag(slope_becomes) | base::is.na(dplyr::lag(slope_becomes)), yes = 1, no = 0))) %>%
    ## Group by that diagnostic and the change type
    dplyr::group_by(change_type, diagnostic) %>%
    ## Summarize
    dplyr::summarise(change_count = dplyr::first(change_count),
                     slope_becomes = dplyr::first(slope_becomes),
                     mean_x = mean(as.numeric(mean_x_v1), na.rm = T),
                     sd_x = mean(as.numeric(sd_x_v1)),
                     n_x = sum(n_x_v1, na.rm = T),
                     se_x = mean(as.numeric(se_x_v1)),
                     .groups = 'keep') %>%
    ## Ungroup
    dplyr::ungroup() %>%
    ## Remove the diagnostic column
    dplyr::select(-diagnostic) %>%
    # Sort from lowest to highest X (again)
    dplyr::arrange(mean_x) %>%
    # Calculate distance to next one
    dplyr::mutate(dist_to_next = dplyr::lead(x = mean_x) - mean_x) %>%
    # Add half the distance between +/0 and 0/- to +/0 to get +/- inflection point
    dplyr::mutate(
      pos_to_neg = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_negative",
                          yes = (mean_x + (dist_to_next / 2)),
                          no = NA),
      neg_to_pos = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_positive",
                          yes = (mean_x + (dist_to_next / 2)),
                          no = NA) ) %>%
    # Remove 'dist to next' column
    dplyr::select(-dist_to_next) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that data object
  return(sizer_data)
}

# Function No. 3 - Identify Inflection Points for ONE Bandwidth ----
## Purpose: Identifies the inflection points (i.e., +/- or -/+ slope changes) for the *one* specified bandwidth. Note that this bandwidth need not be an exact match for what is in the `SiZer` object but should be close 
sizer_slice <- function(sizer_object = NULL, bandwidth = NULL){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | class(sizer_object) != "SiZer")
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Error out if bandwidth isn't provided
  if(is.null(bandwidth)) stop("`bandwidth` must be provided")
  
  # If bandwidth is not a number, coerce it into being one
  if(!is.numeric(bandwidth)){
    bandwidth <- suppressWarnings(as.numeric(bandwidth)) }
  
  # Error out if coercion to numeric deleted bandwidth
  if(is.na(bandwidth))
    stop("`bandwidth` must be numeric (or coercible to numeric)")
  
  # Strip SiZer object content into dataframe
  sizer_raw <- as.data.frame(sizer_object)
  
  # Error out if bandwidth is not included in original sizer_object
  if(bandwidth < base::min(sizer_raw$h) | bandwidth > base::max(sizer_raw$h))
    stop("`bandwidth` is not included in range of bandwidths specified in `SiZer::SiZer` call. Change `bandwidth` or add desired bandwidth to `SiZer::SiZer` call.")
  
  # Perform necessary wrangling
  sizer_data <- sizer_raw %>%
    # Rename columns
    dplyr::rename(x_grid = x, h_grid = h, slope_becomes = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope_becomes != "insufficient data") %>%
    # Find the bandwidth closest to the user-specified value
    dplyr::filter(base::abs(h_grid - bandwidth) == base::min(base::abs(h_grid - bandwidth))) %>%
    # Group within the single bandwidth
    dplyr::group_by(h_grid) %>%
    # Identify whether the next value is the same or different
    dplyr::mutate(transition = dplyr::case_when(
      # First identify start of each group
      is.na(dplyr::lag(slope_becomes, n = 1)) ~ 'start',
      # Now identify whether each value is the same as or different than previous
      slope_becomes == dplyr::lag(slope_becomes, n = 1) ~ 'same',
      slope_becomes != dplyr::lag(slope_becomes, n = 1) ~ 'change'
    )) %>%
    # Filter to retain only those rows that indicate a slope change
    dplyr::filter(transition == "change") %>%
    # Lets also identify what type of change the transition was
    dplyr::mutate(change_type = dplyr::case_when(
      transition == "change" & slope_becomes == "increasing" ~ 'change_to_positive',
      transition == "change" & slope_becomes == "flat" ~ 'change_to_zero',
      transition == "change" & slope_becomes == "decreasing" ~ 'change_to_negative')) %>%
    # Account for if multiple of the same change happen in a curve
    dplyr::group_by(h_grid, change_type) %>%
    dplyr::mutate(change_count = seq_along(unique(x_grid))) %>%
    # Ungroup for subsequent operations
    dplyr::ungroup() %>%
    # Calculate distance to next one
    dplyr::mutate(dist_to_next = dplyr::lead(x = x_grid) - x_grid) %>%
    # Add half the distance between +/0 and 0/- to +/0 to get +/- inflection point
    dplyr::mutate(
      pos_to_neg = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_negative",
                          yes = (x_grid + (dist_to_next / 2)),
                          no = NA),
      neg_to_pos = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_positive",
                          yes = (x_grid + (dist_to_next / 2)),
                          no = NA) ) %>%
    # Remove 'dist to next' column
    dplyr::select(-dist_to_next) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that object
  return(sizer_data) 
  }

# Function No. 4 - SiZer Base Plot ---------------------------------
## Purpose: Creates the base R `plot` returned for `SiZer` objects and adds horizontal lines at the three bandwidths provided in `bandwidth_vec`
sizer_plot <- function(sizer_object = NULL,
                       bandwidth_vec = c(3, 6, 9)){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | class(sizer_object) != "SiZer")
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Make plot
  plot(sizer_object)
  
  # Add horizontal lines at bandwidths of interest
  for(band in bandwidth_vec){ abline(h = log10(band)) }
}

# Function No. 5 - SiZer ggplot ------------------------------------
## Purpose: Creates a `ggplot2` plot of the trendline with slope changes identified by `SiZer` in dashed orange lines and inflection points (i.e., +/- or -/+ slope changes) identified in blue and red respectively
sizer_ggplot <- function(raw_data = NULL, x = NULL, y = NULL,
                         sizer_data = NULL, trendline = FALSE,
                         vline = "all"){
  
  # Error out if these aren't provided
  if(is.null(raw_data) | is.null(sizer_data) |
     is.null(x) | is.null(y))
    stop("All arguments must be provided.")
  
  # Error out if the data are not both dataframes
  if(class(raw_data) != "data.frame" |
     class(sizer_data) != "data.frame") 
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y))
    stop("The x and y columns must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(raw_data) | !y %in% names(raw_data))
    stop("`x` and `y` are not names in the provided `raw_data` object")
  
  # Error out for unsupported vertical intercept call
  if(!vline %in% c("all", "inflections", "changes", "none"))
    stop("`vline` must be one of 'all', 'inflections', 'changes', or 'none'")
  
  # Warning if trendline isn't TRUE/FALSE
  if(class(trendline) != "logical"){
    message("`trendline` must be TRUE or FALSE. Defaulting to FALSE")
    trendline <- FALSE }
  
  # Warning if vline is set to FALSE (rather than "none")
  if(vline == FALSE){
    message("`vline` should be set to 'none' if no x-intercepts are desired. Defaulting to 'none'")
    vline <- "none"
  }
  
  # Now make the actual plot
  p <- ggplot(data = raw_data, aes_string(x = x, y = y)) +
    geom_point() +
    theme_classic()
  
  # Add x-intercepts for slope changes if desired
  
  # Including SiZer-identified inflection points
  if(vline %in% c("all", "changes")){
  p <- p + 
    geom_vline(xintercept = sizer_data$mean_x, color = 'orange',
             linetype = 2, na.rm = TRUE) +
    geom_vline(xintercept = sizer_data$x_grid, color = 'orange',
               linetype = 2, na.rm = TRUE) }
  
  # If `trendline` is TRUE, add the trendline
  if(trendline == TRUE) {
  p <- p +
    geom_smooth(method = 'loess', formula = 'y ~ x', 
                se = FALSE, color = 'black') }
  
  # Add the positive to negative inflection point line(s) if one exists and they are desired
  if(!base::all(is.na(sizer_data$pos_to_neg)) & vline %in% c("all", "inflections")){
    p <- p +
      geom_vline(xintercept = sizer_data$pos_to_neg, color = 'blue',
                 na.rm = TRUE) }
  
  # Add *negative to positive* inflection point line(s) if one exists
  if(!base::all(is.na(sizer_data$neg_to_pos)) & vline %in% c("all", "inflections")){
    p <- p +
      geom_vline(xintercept = sizer_data$neg_to_pos, color = 'red',
                 na.rm = TRUE) }
  
  # Return the plot
  return(p)
}

# Function No. 6 - Identify Inflection Points in Slope -------------
## Purpose: "Breaks" a given continuous variable at the *inflection points* identified by `sizer_aggregate` or `sizer_slice` and creates a column (named `groups`) that contains these group designations. Additionally, it identifies the start and end value of X for each group in two other new columns (named `start` and `end`, respectively)
id_inflections <- function(raw_data = NULL, sizer_data = NULL,
                           x = NULL, y = NULL){
  
  # Error out if these aren't provided
  if(is.null(raw_data) | is.null(sizer_data) | is.null(x) | is.null(y))
    stop("All arguments must be provided.")
  
  # Error out if the data are not both dataframes
  if(class(raw_data) != "data.frame" | class(sizer_data) != "data.frame") 
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y))
    stop("The x and y columns must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(raw_data) | !y %in% names(raw_data))
    stop("`x` and `y` are not names in the provided `raw_data` object")
  
  # Grab inflection points
  brk_pts <- c(sizer_data$pos_to_neg, sizer_data$neg_to_pos)
  
  # Drop NAs
  brk_pts_actual <- brk_pts[!is.na(brk_pts)]

  # Create necessary columns
  data_mod <- raw_data %>%
    # Identify rough groups
    dplyr::mutate(
      groups = base::cut(x = tidyselect::all_of(raw_data[[x]]),
                         breaks = c(-Inf, brk_pts_actual, Inf)),
      .after = tidyselect::all_of(x)) %>%
    # Identify start / end years from the groups
    tidyr::separate(col = groups, sep = ",", remove = FALSE,
                    into = c('rough_start', 'rough_end')) %>%
    # Remove parentheses / brackets
    dplyr::mutate(
      simp_start = stringr::str_sub(
        rough_start, start = 2, end = nchar(rough_start)),
      simp_end = gsub(pattern = "]| ", replacement = "", 
                      x = rough_end)) %>%
    # Swap "Inf" and "-Inf" for the actual start/end X values
    dplyr::mutate(
      start = as.numeric(ifelse(test = (simp_start == -Inf),
                                yes = dplyr::first(raw_data[[x]]),
                                no = simp_start)),
      end = as.numeric(ifelse(test = (simp_end == Inf),
                              yes = dplyr::last(raw_data[[x]]),
                              no = simp_end)),
      .after = groups) %>%
    # Remove intermediary columns
    dplyr::select(-rough_start, -rough_end,
                  -simp_start, -simp_end) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that modified dataframe
  return(data_mod) }

# Function No. 6 - Identify Inflection Points in Slope -------------
## Purpose: "Breaks" a given continuous variable at the *slope changes* identified by `sizer_aggregate` or `sizer_slice` and creates a column (named `groups`) that contains these group designations. Additionally, it identifies the start and end value of X for each group in two other new columns (named `start` and `end`, respectively)
id_slope_changes <- function(raw_data = NULL, sizer_data = NULL,
                             x = NULL, y = NULL){
  
  # Error out if these aren't provided
  if(is.null(raw_data) | is.null(sizer_data) | is.null(x) | is.null(y))
    stop("All arguments must be provided")
  
  # Error out if the data are not both dataframes
  if(class(raw_data) != "data.frame" | class(sizer_data) != "data.frame") 
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y))
    stop("The x and y columns must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(raw_data) | !y %in% names(raw_data))
    stop("`x` and/or `y` is not in the provided `raw_data` object")
  
  # Rename the aggregate X column to make downstream operations easier
  if("mean_x" %in% names(sizer_data)){
    sizer_data <- sizer_data %>%
      dplyr::rename(x_grid = mean_x) }
  
  # Grab slope change points
  brk_pts <- c(sizer_data$x_grid)
  
  # Drop NAs
  brk_pts_actual <- brk_pts[!is.na(brk_pts)]
  
  # Make a simpler version of the sizer data (we'll need this later)
  sizer_simp <- sizer_data %>%
    # Break X by breakpoints identified in this dataframe
    dplyr::mutate(groups = base::cut(x = sizer_data$x_grid,
                                     breaks = c(-Inf, brk_pts_actual, Inf))) %>%
    # Identify what the slope *is* (rather than what it changes to)
    dplyr::mutate(slope_type_rough = dplyr::case_when(
      slope_becomes == "flat" ~ "increasing/decreasing",
      slope_becomes != "flat" ~ "approx. zero") ) %>%
    # Crop to needed columns
    dplyr::select(groups, change_type, slope_type_rough) %>%
    as.data.frame()
  
  # Create necessary columns
  data_mod <- raw_data %>%
    # Identify rough groups
    dplyr::mutate(
      groups = base::cut(x = raw_data[[x]],
                         breaks = c(-Inf, brk_pts_actual, Inf)),
      .after = tidyselect::all_of(x)) %>%
    # Attach slope types from simplified SiZer object
    dplyr::left_join(y = sizer_simp, by = "groups") %>%
    # Fill through the last group (it is only implied by the `cut` groups)
    tidyr::fill(change_type) %>%
    # Combine that column with the `slope_type_rough` column
    dplyr::mutate(slope_type = dplyr::case_when(
      !is.na(slope_type_rough) ~ slope_type_rough,
      is.na(slope_type_rough) &
        change_type == "change_to_zero" ~ "approx. zero",
      is.na(slope_type_rough) &
        change_type != "change_to_zero" ~ "increasing/decreasing"),
      .after = groups) %>%
    # Remove intermediary columns
    dplyr::select(-change_type, -slope_type_rough) %>%
    # Identify start / end years from the groups
    tidyr::separate(col = groups, sep = ",", remove = FALSE,
                    into = c('rough_start', 'rough_end')) %>%
    # Remove parentheses / brackets
    dplyr::mutate(
      simp_start = stringr::str_sub(
        rough_start, start = 2, end = nchar(rough_start)),
      simp_end = gsub(pattern = "]| ", replacement = "", 
                      x = rough_end)) %>%
    # Swap "Inf" and "-Inf" for the actual start/end X values
    dplyr::mutate(
      start = as.numeric(ifelse(test = (simp_start == -Inf),
                                yes = dplyr::first(raw_data[[x]]),
                                no = simp_start)),
      end = as.numeric(ifelse(test = (simp_end == Inf),
                              yes = dplyr::last(raw_data[[x]]),
                              no = simp_end)),
      .after = groups) %>%
    # Remove intermediary columns
    dplyr::select(-rough_start, -rough_end,
                  -simp_start, -simp_end) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that modified dataframe
  return(data_mod) }

# Function No. 8 - Linear Models with SiZer ------------------------
## Purpose: fits separate linear models for each group of the line between the break points identified by `id_inflections` or `id_slope_changes`. Returns a list containing (1) the model summary statistics and (2) the the estimates of the intercept and line
sizer_lm <- function(data = NULL, x = NULL, y = NULL,
                     group_col = NULL){
  
  # Error out if these aren't provided
  if(is.null(data) | is.null(group_col) | is.null(x) | is.null(y))
    stop("All arguments must be provided.")
  
  # Error out if the data are not both dataframes
  if(class(data) != "data.frame") 
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y) | !is.character(group_col))
    stop("`x`, `y`, and `group_col` must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(data) | !y %in% names(data) | !group_col %in% names(data))
    stop("At least one of `x`, `y`, or `group_col` are not names in the provided `data` object")
  
  # Handle plots without break points
  if(length(unique(data[[group_col]])) == 1){
    # Just fit model
    model_fit <- lm(data[[y]] ~ data[[x]])
    
    # Identify statistics and estimates
    stat_df <- data.frame(broom::glance(model_fit)) %>%
      dplyr::mutate(section = "No inflection points",
                    .before = dplyr::everything())
    est_df <- data.frame(broom::tidy(model_fit)) %>%
      dplyr::mutate(section = "No inflection points",
                    .before = dplyr::everything())
    
    # Make an empty list
    return_list <- list()
    
    # Put the data in it
    return_list[["Stat"]] <- stat_df
    return_list[["Estim"]] <- est_df
    
    # Return it
    return(return_list)
    
    # If there *are* break points
  } else {
    
    # Make an empty list to store each model
    model_fit_list <- list()
    
    # Fit a model for each
    for(chunk in unique(data[[group_col]])){
      
      # Fit model
      chunk_fit <- model_fit <- lm(data[[y]] ~ data[[x]], subset = data[[group_col]] == chunk)
      
      # Grab statistics
      chk_stat_df <- data.frame(broom::glance(chunk_fit)) %>%
        dplyr::mutate(section = chunk, .before = dplyr::everything())
      
      # Grab estimates
      chk_est_df <- data.frame(broom::tidy(chunk_fit)) %>%
        dplyr::mutate(section = chunk, .before = dplyr::everything())
      
      # Add it to the list
      model_fit_list[[paste0("Stats_", chunk)]] <- chk_stat_df
      model_fit_list[[paste0("Estimates_", chunk)]] <- chk_est_df
    }
    
    # Unlist the statistic list
    stat_bit <- model_fit_list %>%
      # Identify all list elements that contain stats info
      purrr::keep(.p = stringr::str_detect(string = names(.),
                                           pattern = "Stats")) %>%
      # Unlist by selecting all columns of each list element
      purrr::map_dfr(.f = dplyr::select, dplyr::everything())
    
    # Unlist the estimate list too
    est_bit <- model_fit_list %>%
      purrr::keep(.p = stringr::str_detect(string = names(.), pattern = "Estimates")) %>%
      purrr::map_dfr(.f = dplyr::select, dplyr::everything())
    
    # Now that we have these as two complete dataframes (rather than however many "chunks" there were) we'll add them *back into a new list!*
    
    # Make an empty list
    return_list <- list()
    
    # Put the data in it
    return_list[["Stat"]] <- stat_bit
    return_list[["Estim"]] <- est_bit
    
    # Return it
    return(return_list) }
}

# End ----
