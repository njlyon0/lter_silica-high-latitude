#' @title Identify LTER Counts
#' 
#' @description Creates a data table object containing the number of streams in each LTER. This can be easily used to add dividing lines to graphs that separate streams within an LTER from streams from different LTERs.
#' 
#' @param data (data.frame) data object from which to create a bookmark graph
#'
lter_ct <- function(data = NULL){
  
  # Errors for missing arguments
  if(is.null(data) == TRUE)
    stop("'data' argument must be specified")
  
  # Error for wrong data type
  if(all(class(data) %in% c("data.frame", "tbl", "tbl_df")) == FALSE)
    stop("'data' must be a dataframe / dataframe-like object")
  
  # Error for not all needed columns included
  if(all(c('LTER', 'Stream_Name', 'Year', 'LTER_stream') %in% names(data)) != TRUE)
    stop("'data' must include all of the following columns: 'LTER', 'Stream_Name', 'Year', 'LTER_stream'")
  
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
    dplyr::mutate(line_positions = stream_cumulative + 0.5)
  
  # Return that
  return(group_lines)
  
}

# End ----
