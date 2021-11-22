#' Get regression coefficients from a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' coef(fit)
#' @export 

coef.linreg <- function(object, ...) {
  if(class(object) != "linreg"){
    stop("Error: input linreg object")
  }
  return(object[["Coefficients"]])
}

coef.ridgereg <- function(object, ...) {
  if(class(object) != "ridgereg"){
    stop("Error: input linreg object")
  }
  return(object[["Coefficients"]])
}

