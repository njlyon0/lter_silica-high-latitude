#' @title Check Whether Variables are in Data
#' 
#' @description This function checks whether the supplied variables are found in the provided dataframe. In some cases variables are assumed to be columns while in others we are checking the contents of a particular column
#' 
#' @param data (data.frame) data object in which to look for the provided variables
#' @param chem (character) name of chemical of interest (e.g., "DIN", "DSi", etc.)
#' @param resp_var (character) column name of response variable
#' @param exp_var (character) column name of explanatory variable
#' 
var_check <- function(data = NULL, chem = NULL,
                      resp_var = NULL, exp_var = NULL){
  
  # Error out for any missing arguments
  if(any(is.null(data) | is.null(chem) | is.null(resp_var) | is.null(exp_var)))
    stop("All arguments must be specified")
  
  # Error for wrong data type
  if(all(class(data) %in% c("data.frame", "tbl", "tbl_df")) == FALSE)
    stop("'data' must be a dataframe / dataframe-like object")
  
  # Error for wrong class of other arguments
  if(is.character(chem) != TRUE)
    stop("'chem' must be a character")
  if(is.character(resp_var) != TRUE)
    stop("'resp_var' must be a character")
  if(is.character(exp_var) != TRUE)
    stop("'exp_var' must be a character")
  
  # Error for explanatory/response vars not found in data
  ## Response
  if(!resp_var %in% names(data)){
    stop("Response not found in data! Check spelling") 
  } else { message("Response looks good!") }
  ## Explanatory
  if(!exp_var %in% names(data)) {
    stop("Explanatory variable not found in data! Check spelling")
  } else { message("Explanatory variable looks good!") }
  
  # Error for chemical not found in data
  if(!chem %in% unique(data$chemical)){
    stop("Chemical not found in data! Check spelling.")
  } else { message("Chemical looks good!") }
  
  # Subset the data given those parameters
  data_sub <- dplyr::filter(.data = data, chemical == chem)
 
  # Error for no data in those variables
  if(all(is.na(data_short[[resp_var]])) == TRUE){
    stop("No non-NA response values found for this chemical")
  } else { message("Data subset looks good!") }
  
}

# End ----
