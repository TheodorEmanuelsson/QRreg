#' Get residuals from a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments that we don't use
#' @description Issue with the resid function: Does not work as resid.linreg for some odd reason. Must mask from stats package
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' resid(fit)
#' @export 

residuals.linreg <- function(object, ...) {
  if(class(object) != "linreg"){
    stop("Error: input linreg object")
  }
  return(object[["Residuals"]])
}
