#' @title Make a Bookmark Graph
#' 
#' @description Creates a 'bookmark graph' where SiZer information about each stream's data is relayed in a time series line. The color of the line indicates something about the direction of change and/or significance of that line segment.
#' 
#' @param data (data.frame) data object from which to create a bookmark graph
#' @param color_var (character) name of column in 'data' to color lines by
#' @param colors named vector of colors to use within bookmark graph lines
#'
#'
bookmark_graph <- function(data = NULL, color_var = NULL, colors = NULL){
  
  # Errors for missing arguments
  if(any(is.null(data) | is.null(color_var) | is.null(colors)))
    stop("All arguments must be specified")
  
  # Error for wrong data type
  if(all(class(data) %in% c("data.frame", "tbl", "tbl_df")) == FALSE)
    stop("'data' must be a dataframe / dataframe-like object")
  
  # Error for wrong class of response variable
  if(is.character(color_var) != TRUE | is.character(colors) != TRUE)
    stop("'color_var' and 'colors' must be characters")
  
  # Error for not all needed columns included
  if(all(c('LTER', 'stream', 'Year', 'LTER_stream', color_var) %in% names(data)) != TRUE)
    stop(paste0("'data' must include all of the following columns: 'LTER', 'stream', 'Year', 'LTER_stream', ", color_var))
  
  # Identify placement of group lines for graph
  group_lines <- data %>%
    # Pare down to desired columns / rows
    dplyr::select(LTER, stream) %>%
    dplyr::distinct() %>%
    # Count the number of streams / LTER
    dplyr::group_by(LTER) %>%
    dplyr::summarize(stream_ct = dplyr::n()) %>%
    dplyr::ungroup() %>%
    # Count cumulative streams across LTERs
    dplyr::mutate(stream_cumulative = cumsum(x = stream_ct)) %>% 
    # Nudge those up by 0.5 to avoid intersecting data
    dplyr::mutate(line_pos = stream_cumulative + 0.5)
  
  # Create desired graph
  bookmark <- ggplot(data = data, mapping = aes(x = Year, y = LTER_stream, 
                                                color = .data[[color_var]])) +
    # Fat / lines to make the illusion of bars
    geom_path(mapping = aes(group = sizer_groups), lwd = 3.5, lineend = 'square') +
    # Manual color specification
    scale_color_manual(values = colors) +
    # Put in horizontal lines between LTERs
    geom_hline(yintercept = group_lines$line_pos) +
    # Customize theme / formatting elements
    labs(x = "Year", y = "Stream")
  
  # Make needed tweaks for temporal resolution of data
  ## Monthly data
  if(all(unique(data$Month) == "x") != TRUE){
    
    # Facet by month
    bookmark_actual <- bookmark +
      facet_wrap(. ~ Month)
    
    ## Seasonal data
  } else if (all(unique(data$Month) == "x") != TRUE){
    
    # Facet by season
    bookmark_actual <- bookmark +
      facet_grid(. ~ season)
    
    ## Annual data is only data that won't meet preceding conditionals
  } else { bookmark_actual <- bookmark }
  
  # Return graph
  return(bookmark_actual)
  
}

# End ----
