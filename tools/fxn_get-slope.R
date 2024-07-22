#' @title Identify Slope of Line for Provided Response
#' 
#' @description This function fits a simple linear model with the specified response variable against 'relative_Year' (assumed to be in the provided data object) and returns the slope of that line. If there are all rows of the response variable are NA, returns NA instead for the slope.
#' 
#' @param data (data.frame) data object in which to look for the provided variables
#' @param resp_var (character) column name of response variable
#' 
get_slope <- function(data = NULL, resp_var = NULL){
  
  # Errors for missing arguments
  if(any(is.null(data) | is.null(resp_var)))
    stop("All arguments must be specified")
  
  # Error for wrong data type
  if(all(class(data) %in% c("data.frame", "tbl", "tbl_df")) == FALSE)
    stop("'data' must be a dataframe / dataframe-like object")
  
  # Error for wrong class of response variable
  if(is.character(resp_var) != TRUE)
    stop("'resp_var' must be a character")
  
  # Error for not all needed columns included
  if(all(c("relative_Year", resp_var) %in% names(data)) != TRUE)
    stop("'data' must include a column for the response variable AND a 'relative_Year' column")
  
  # If all values are NA...
  if(all(is.na(data[[resp_var]])) == TRUE){
    
    # Return NA
    slope <- NA
    
    # Otherwise...
  } else {
    
    # Fit model
    mod <- lm(data[[resp_var]] ~ relative_Year, data = data)
    
    # Extract coefficients
    coef <- summary(mod)$coefficients
    
    # Strip out the slope
    slope <- as.data.frame(coef)$Estimate[2]
    
  }
  
  # Return the slope
  return(slope)
  
}

# End ----
