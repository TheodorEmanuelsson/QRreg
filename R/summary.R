#' Get a summary of a linreg object
#'
#' @param object An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' summary(fit)
#' @export 
summary.linreg <- function(object, ...) {
  if(class(object) != "linreg"){
    stop("Error: input linreg object")
  }
  # Call section: Deprecated
  #cat("Call:\n")
  #cat("linreg(formula = ",deparse(linregobject[["formula"]]), ", data = ", linregobject[["dataname"]],")\n\n", sep = "")
  # Residuals section: Deprecated
  #quartiles <- quantile(linregobject[["Residuals"]])
  #names(quartiles) <- c("Min", "Q1", "Median", "Q3", "Max")
  #cat("Residuals:\n")
  #print(quartiles)
  #cat("\n")
  # Coefficients section
  ######## NOTE DOESNT WORK WITH THE TESTING AS A MATRIX
  #coefftable <- matrix(NA, nrow = length(linregobject[["Coefficients"]]), ncol = 5)
  #rownames(coefftable) <- names(linregobject[["Coefficients"]])
  #colnames(coefftable) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
  #coefftable[,1] <- round(linregobject[["Coefficients"]], digits = 5)
  #coefftable[,2] <- round(linregobject[["Coefficient standard deviation"]], digits = 5)
  #coefftable[,3] <- round(linregobject[["t-values"]], digits = 3)
  #coefftable[,4] <- format(linregobject[["p-values"]], digits = 3)
  #coefftable[,5] <- categorize_pvalues(linregobject[["p-values"]])
  #print(coefftable)
  #cat("---\n")
  #cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  #cat("\n")
  #cat(paste("Residual standard error: ", round(linregobject[["Residual SE"]], digits =  4), " on ", linregobject[["df"]], "degrees of freedom"))
  ######## NEW PRINTING APPROACH
  coef_names <- names(object[["Coefficients"]])
  betas <- round(object[["Coefficients"]], digits = 5)
  std <- round(object[["Coefficient standard deviation"]], digits = 5)
  tval <- round(object[["t-values"]], digits = 3)
  pval <- format(object[["p-values"]], digits = 3)
  pvalcat <- categorize_pvalues(object[["p-values"]])
  residSE <- round(object[["Residual SE"]], digits =  4)
  i <- 1
  repeat{
    cat(coef_names[i], betas[i], std[i], tval[i], pval[i], pvalcat[i],"\n", sep = " ")
    i <- i + 1
    if(i > length(names(object[["Coefficients"]]))){break}
  }
  cat(paste("Residual standard error:", residSE , "on", object[["df"]], "degrees of freedom"))
}
