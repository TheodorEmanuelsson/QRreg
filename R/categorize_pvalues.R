#' Categorize p-values for beta coefficients in printing the summary() of a linreg object
#'
#' @description Internal use in the summary.linreg() function
#' @param linregobject An object of class linreg
#' 
#' @keywords internal
#' @export 

categorize_pvalues <- function(p_values){
  output <- vector()
  for (i in 1:length(p_values)) {
    if(p_values[i] > 0.1){
      output[i] <- ""
    } else if(p_values[i] > 0.05){
      output[i] <- "."
    } else if(p_values[i] > 0.01){
      output[i] <- "*"
    } else if(p_values[i] > 0.001){
      output[i] <- "**"
    } else{
      output[i] <- "***"
    } 
  }
  return(output)
}

