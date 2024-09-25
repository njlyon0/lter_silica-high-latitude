#' @title Make a 'Pick up Sticks' Graph
#' 
#' @description Creates a 'pick up sticks' graph where two continuous values are plotted against each other and--if a significant interaction is specified--the best-fit lines for each group are included. Also has options for a single line and no lines at all. Interaction is always assumed to be with LTER
#' 
#' @param data (data.frame) data object from which to create a bookmark graph
#' @param exp_var (character) name of column in 'data' to map to X axis
#' @param resp_var (character) name of column in 'data' to map to Y axis
#' @param sig (character) one of "ixn", "main", or "NS" for whether the interaction was significant, only the main effect was significant, or no significance was found respectively
#'
#'
stick_graph <- function(data = NULL, exp_var = NULL, resp_var = NULL, sig = NULL){
  
  # Errors for missing arguments
  if(any(is.null(data) | is.null(exp_var) | is.null(resp_var) | is.null(sig)))
    stop("All arguments must be specified")
  
  # Error for wrong data type
  if(all(class(data) %in% c("data.frame", "tbl", "tbl_df")) == FALSE)
    stop("'data' must be a dataframe / dataframe-like object")
  
  # Error for wrong class of variables
  if(is.character(exp_var) != TRUE | is.character(resp_var) != TRUE)
    stop("'exp_var' and 'resp_var' must be characters")
  
  # Error for typos in variables
  if(!exp_var %in% names(data) | !resp_var %in% names(data))
    stop("'exp_var' and 'resp_var' must be column names in 'data'")
  
  # Error for inappropriate value to 'sig'
  if(sig %in% c("ixn", "main", "NS") != TRUE)
    stop("'sig' must be one of 'ixn', 'main', or 'NS'")
  
  # Generate core bit of pick up sticks plot
  core_stick <- ggplot(data = data, aes(x = .data[[exp_var]], y = .data[[resp_var]]))
  
  # If interaction is signficant, color by LTER
  if(sig == "ixn"){
    
    multi_sticks <- core_stick +
      geom_smooth(aes(color = LTER), formula = "y ~ x", method = "lm", se = F) +
      geom_point(aes(fill = LTER, shape = LTER), size = 2) +
      scale_color_manual(values = lter_palt) +
      scale_fill_manual(values = lter_palt) +
      scale_shape_manual(values = lter_shps)
    
    # If only main effect is sig, fit just one line
  } else if (sig == "main") {
    
    multi_sticks <- core_stick +
      geom_smooth(color = "black", formula = "y ~ x", method = "lm", se = F) +
      geom_point(fill = "gray", pch = 21, size = 2)
    
    # Otherwise, just add points
  } else {
    
    multi_sticks <- core_stick +
      geom_point(fill = "gray", pch = 21, size = 2)
    
  }
  
  # Add typical theme elements regardless of significance
  stick_actual <- multi_sticks +
    theme(panel.background = element_blank(),
          axis.line = element_line(color = "black"),
          legend.title = element_blank())
  
  # Return this
  return(stick_actual)
  
}

# End ----
