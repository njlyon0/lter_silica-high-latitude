#' @title Make a Bookmark Graph with Points Instead of Bars
#' 
#' @description Creates a 'bookmark graph' where SiZer information about each stream's data is relayed in a dotted time series line. The color of the line indicates something about the direction of change and/or significance of that line segment.
#' 
#' @param data (data.frame) data object from which to create a bookmark graph
#' @param resp_var (character) name of column in 'data' to color lines by
#' @param colors named vector of colors to use within bookmark graph lines
#'
bookmark_points <- function(data = NULL, resp_var = NULL, 
                            colors = NULL, shapes = NULL){
  
  # Errors for 'data' argument
  if(is.null(data) || "data.frame" %in% class(data) != T ||
     all(c('LTER', 'Stream_Name', 'Year', 'LTER_stream', resp_var) %in% names(data)) != T)
    stop(paste0("'data' must be a dataframe-like object including all of the following columns: 'LTER', 'Stream_Name', 'Year', 'LTER_stream', ", resp_var))
  
  # Error for 'resp_var' argument
  if(is.null(resp_var) || is.character(resp_var) != T || colo_var %in% names(data) != T)
    stop("'resp_var' must be a column name in 'data'")
  
  # Errors for 'colors' argument
  if(is.null(colors) || is.character(colors) != T || length(colors) != length(unique(data[[resp_var]])))
    stop("'colors' must be a character vector of same length as unique 'resp_var' values")
  
  # Errors for 'shapes' argument
  if(is.null(shapes) || is.character(shapes) != T || length(colors) != length(unique(data[[resp_var]])))
    stop("'shapes' must be a character vector of same length as unique 'resp_var' values")
  
  # Identify placement of group lines for graph
  group_lines <- data %>%
    # Pare down to desired columns / rows
    dplyr::select(LTER, Stream_Name) %>%
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
                                                color = .data[[resp_var]])) +
    # Points
    geom_point(mapping = aes(group = sizer_groups, shape = .data[[resp_var]]), size = 2) +
    # Manual color specification
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
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
