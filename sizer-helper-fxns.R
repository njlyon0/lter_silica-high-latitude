# Read in needed libraries
library(SiZer, tidyverse)

# Function No. 1 - Extract and Average -----------------------------
sizer_extract <- function(sizer_object = NULL){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | class(sizer_object) != "SiZer")
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Strip SiZer object content into dataframe
  sizer_raw <- as.data.frame(sizer_object)
  
  # Perform necessary wrangling
  sizer_data <- sizer_raw %>%
    # Rename columns
    dplyr::rename(x_grid = x, h_grid = h, slope = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope != "insufficient data") %>%
    # Within bandwidth levels (h_grid)
    dplyr::group_by(h_grid) %>%
    # Identify whether the next value is the same or different
    dplyr::mutate(transition = dplyr::case_when(
      # First identify start of each group
      is.na(dplyr::lag(slope, n = 1)) ~ 'start',
      # is.na(dplyr::lead(slope, n = 1)) ~ 'end',
      # Now identify whether each value is the same as or different than previous
      slope == dplyr::lag(slope, n = 1) ~ 'same',
      slope != dplyr::lag(slope, n = 1) ~ 'change'
      # slope == dplyr::lead(slope, n = 1) ~ 'no'
    )) %>%
    # Filter to retain only those rows that indicate a slope change
    dplyr::filter(transition == "change") %>%
    # Lets also identify what type of change the transition was
    dplyr::mutate(change_type = dplyr::case_when(
      transition == "change" & slope == "increasing" ~ 'change_to_positive',
      transition == "change" & slope == "flat" ~ 'change_to_zero',
      transition == "change" & slope == "decreasing" ~ 'change_to_negative')) %>%
    # Account for if multiple of the same change happen in a curve
    dplyr::group_by(h_grid, change_type) %>%
    dplyr::mutate(change_count = seq_along(unique(x_grid))) %>%
    # Ungroup for subsequent operations
    dplyr::ungroup() %>%
    # Group by change type
    dplyr::group_by(change_count, change_type) %>%
    # And average the x_grid value
    dplyr::summarise(slope = dplyr::first(slope),
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
    dplyr::mutate(diagnostic = cumsum(ifelse(slope != dplyr::lag(slope) | base::is.na(dplyr::lag(slope)), yes = 1, no = 0))) %>%
    ## Group by that diagnostic and the change type
    dplyr::group_by(change_type, diagnostic) %>%
    ## Summarize
    dplyr::summarise(change_count = dplyr::first(change_count),
                     slope = dplyr::first(slope),
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
                          no = NA)
    ) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that data object
  return(sizer_data)
}

# Function No. 2 - Still Average but Better Named ------------------
sizer_aggregate <- function(sizer_object = NULL){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | class(sizer_object) != "SiZer")
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Strip SiZer object content into dataframe
  sizer_raw <- as.data.frame(sizer_object)
  
  # Perform necessary wrangling
  sizer_data <- sizer_raw %>%
    # Rename columns
    dplyr::rename(x_grid = x, h_grid = h, slope = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope != "insufficient data") %>%
    # Within bandwidth levels (h_grid)
    dplyr::group_by(h_grid) %>%
    # Identify whether the next value is the same or different
    dplyr::mutate(transition = dplyr::case_when(
      # First identify start of each group
      is.na(dplyr::lag(slope, n = 1)) ~ 'start',
      # is.na(dplyr::lead(slope, n = 1)) ~ 'end',
      # Now identify whether each value is the same as or different than previous
      slope == dplyr::lag(slope, n = 1) ~ 'same',
      slope != dplyr::lag(slope, n = 1) ~ 'change'
      # slope == dplyr::lead(slope, n = 1) ~ 'no'
    )) %>%
    # Filter to retain only those rows that indicate a slope change
    dplyr::filter(transition == "change") %>%
    # Lets also identify what type of change the transition was
    dplyr::mutate(change_type = dplyr::case_when(
      transition == "change" & slope == "increasing" ~ 'change_to_positive',
      transition == "change" & slope == "flat" ~ 'change_to_zero',
      transition == "change" & slope == "decreasing" ~ 'change_to_negative')) %>%
    # Account for if multiple of the same change happen in a curve
    dplyr::group_by(h_grid, change_type) %>%
    dplyr::mutate(change_count = seq_along(unique(x_grid))) %>%
    # Ungroup for subsequent operations
    dplyr::ungroup() %>%
    # Group by change type
    dplyr::group_by(change_count, change_type) %>%
    # And average the x_grid value
    dplyr::summarise(slope = dplyr::first(slope),
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
    dplyr::mutate(diagnostic = cumsum(ifelse(slope != dplyr::lag(slope) | base::is.na(dplyr::lag(slope)), yes = 1, no = 0))) %>%
    ## Group by that diagnostic and the change type
    dplyr::group_by(change_type, diagnostic) %>%
    ## Summarize
    dplyr::summarise(change_count = dplyr::first(change_count),
                     slope = dplyr::first(slope),
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
sizer_slice <- function(sizer_object = NULL, bandwidth = NULL){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | class(sizer_object) != "SiZer")
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Error out if bandwidth isn't provided
  if(is.null(bandwidth)) stop("`bandwidth` must be provided")
  
  # If bandwidth is not a number, coerce it into being one
  if(!is.numeric(bandwidth)){
    bandwidth <- suppressWarnings(as.numeric(bandwidth)) }
  
  # Error out if coersion to numeric deleted bandwidth
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
    dplyr::rename(x_grid = x, h_grid = h, slope = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope != "insufficient data") %>%
    # Find the bandwidth closest to the user-specified value
    dplyr::filter(base::abs(h_grid - bandwidth) == base::min(base::abs(h_grid - bandwidth))) %>%
    # Group within the single bandwidth
    dplyr::group_by(h_grid) %>%
    # Identify whether the next value is the same or different
    dplyr::mutate(transition = dplyr::case_when(
      # First identify start of each group
      is.na(dplyr::lag(slope, n = 1)) ~ 'start',
      # Now identify whether each value is the same as or different than previous
      slope == dplyr::lag(slope, n = 1) ~ 'same',
      slope != dplyr::lag(slope, n = 1) ~ 'change'
    )) %>%
    # Filter to retain only those rows that indicate a slope change
    dplyr::filter(transition == "change") %>%
    # Lets also identify what type of change the transition was
    dplyr::mutate(change_type = dplyr::case_when(
      transition == "change" & slope == "increasing" ~ 'change_to_positive',
      transition == "change" & slope == "flat" ~ 'change_to_zero',
      transition == "change" & slope == "decreasing" ~ 'change_to_negative')) %>%
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
sizer_ggplot <- function(raw_data = NULL, x = NULL, y = NULL,
                         sizer_data = NULL){
  
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
  
  # Now make the actual plot
  p <- ggplot(data = raw_data, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = 'loess', formula = 'y ~ x', 
                se = F, color = 'black') +
    # Including SiZer-identified inflection points
    geom_vline(xintercept = sizer_data$mean_x, color = 'orange',
               linetype = 2, na.rm = TRUE) +
    geom_vline(xintercept = sizer_data$x_grid, color = 'orange',
               linetype = 2, na.rm = TRUE) +
    # And a theme
    theme_classic()
  
 
  # Add the positive to negative inflection point line(s) if one exists
  if(!base::all(is.na(sizer_data$pos_to_neg))){
    p <- p +
      geom_vline(xintercept = sizer_data$pos_to_neg, color = 'blue',
                 na.rm = TRUE) }
  
  # Add *negative to positive* inflection point line(s) if one exists
  if(!base::all(is.na(sizer_data$neg_to_pos))){
    p <- p +
      geom_vline(xintercept = sizer_data$neg_to_pos, color = 'red',
                 na.rm = TRUE) }
  
  # Return the plot
  return(p)
}

# Function No. 6 - Linear Models with SiZer ------------------------
sizer_lm <- function(raw_data = NULL, x = NULL, y = NULL,
                     sizer_data = NULL){
  
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
  
  # Grab inflection points
  brk_pts <- c(sizer_data$pos_to_neg, sizer_data$neg_to_pos)
  
  # Drop NAs
  brk_pts_actual <- brk_pts[!is.na(brk_pts)]
  brk_pts_actual
  
  # Handle plots without break points
  if(length(brk_pts_actual) == 0){
    # Just fit model
    model_fit <- lm(raw_data[[y]] ~ raw_data[[x]])
    
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
    
    # Identify rough groups
    data_mod <- raw_data %>%
      dplyr::mutate(groups_rough = base::cut(x = Year,
                                 breaks = c(-Inf, brk_pts_actual, Inf)))
    
    # Fit a model for each
    for(chunk in unique(data_mod$groups_rough)){
      
      # Fit model
      chunk_fit <- model_fit <- lm(data_mod[[y]] ~ data_mod[[x]],
                      subset = data_mod$groups_rough == chunk)
      
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
