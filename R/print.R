#' Print for a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' print(fit)
#' @export 

print.linreg = function(x, ...) {
  if(class(x) != "linreg"){
    stop("Error: input linreg object")
  }
  
  cat("Call:\n")
  cat("linreg(formula = ",deparse(x[["formula"]]), ", data = ", x[["dataname"]],")\n\n", sep = "")
  cat("Coefficients:\n")
  print(x[["Coefficients"]])
  
}

print.ridgereg <- function(x, ...) {
  if(class(x) != "ridgereg"){
    stop("Error: input ridgereg object")
  }
  cat("Call:\n")
  if(x[["QR"]] == FALSE){
    cat("ridgereg(formula = ",deparse(x[["formula"]]), ", data = ", x[["dataname"]],", lambda = ", x[["lambda"]], ")\n\n" , sep = "") 
  } else if(x[["QR"]] == TRUE){
    cat("ridgereg(formula = ",deparse(x[["formula"]]), ", data = ", x[["dataname"]],", lambda = ", x[["lambda"]],",", " use_QR = TRUE)\n\n" , sep = "")
  }
  cat("Coefficients:\n")
  print(x[["Coefficients"]][1:3])
}