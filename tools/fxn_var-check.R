#' @title Check Whether Variables are in Data
#' 
#' @description This function checks whether the supplied variables are found in the provided dataframe. In some cases variables are assumed to be columns while in others we are checking the contents of a particular column
#' 
#' @param data (dataframe) The data object in which to look for the provided variables
#' 
#' 
#' 
var_check <- function(data = NULL, chem = NULL,
                      resp_var = NULL, exp_var = NULL){
  
  # Error for wrong data type
  if(all(class(data) %in% c("data.frame", "tbl", "tbl_df")) == FALSE)
    stop("'data' must be a dataframe / dataframe-like object")
  
  
}

# End ----
