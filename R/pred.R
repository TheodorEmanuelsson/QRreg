#' Get fitted values from a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' predict(fit)
#' @export

predict.linreg <- function(object, ...) {
  if(class(object) != "linreg"){
    stop("Error: input linreg object")
  }
  return(object[["Fitted values"]])
}

predict.ridgereg <- function(object, newdata = NULL , ...) {
  stopifnot(class(object) == "ridgereg",
            is.null(newdata) == TRUE || is.matrix(newdata))
  if(is.null(newdata)){
    return(object[["Fitted values"]])
  }else{
    find_covariates <- colnames(newdata) %in% names(object[["Coefficients"]])[2:3]
    new_df <- scale(newdata[,find_covariates]) # Scale the new data
    fitval <- object$Coefficients[1,] + new_df %*% c(object[["Coefficients"]][2,],object[["Coefficients"]][3,])
    return(fitval) 
  }
}
