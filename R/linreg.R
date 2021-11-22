#' Compute multiple regression using QR decomposition
#'
#' @param formula A model formula
#' @param data A dataframe
#' @return A linreg object
#' @examples
#' fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' @importFrom stats model.matrix pt
#' @export 

linreg <- function(formula, data) {
  if (class(formula) != "formula") {
    stop("Error: Input not valid formula")
  }
  if (is.data.frame(data) != TRUE){
    stop("Error: Invalid data")
  }
  
  ## Model matrix and dependent variable
  X <- model.matrix(formula, data)
  Y_name <- all.vars(formula)[1]
  Y <- as.matrix(data[Y_name])
  
  # Decompose by QR: X = QR for any full rank N x p matrix
  # Q should be an N x p matrix: Q^tQ = 1 (ortogonal)
  # R should be a p x p upper triangular matrix
  # REF for QR decomp: https://genomicsclass.github.io/book/pages/qr_and_regression.html
  QR <-qr(x = X) # Compute the QR decomposition
  Q <- qr.Q(qr = QR) # Returns the Q from our decomposition
  R <- qr.R(qr = QR) # Returns the R from out decomposition
  # Model coefficients
  beta_hat <- as.numeric(backsolve(r = R, x = crossprod(Q, Y)))
  names(beta_hat) <- colnames(X) # Name the vector for use in print() and other functions
  # Fitted values
  fit_val <- as.numeric(X %*% beta_hat)
  names(fit_val) <- as.character(1:length(Y))
  # Residuals, degrees of freedom and variance of the residuals
  residual <- as.numeric(Y - X %*% beta_hat)
  names(residual) <- as.character(1:length(Y))
  df <- nrow(data) - ncol(X)
  resid_var <- as.numeric((t(residual) %*% residual) / df)
  resid_SE <- sqrt(resid_var)
  resid_standardized <- residual / sqrt((1/(nrow(X)-1)) * sum(residual^2))
  resid_standardized_sqrt <- sqrt(abs(resid_standardized))
  # Variance/SD of the regression coefficients
  beta_var <- resid_var * chol2inv(R)
  beta_SE <- sqrt(diag(beta_var))
  # Statistics
  t_val <- beta_hat / beta_SE
  p_val <- 2 * pt(q = -abs(t_val), df)
  
  
  output <- list(
    "Coefficients" = beta_hat,
    "Fitted values" = fit_val,
    "Residuals" = residual,
    "df" = df,
    "Residual variance" = resid_var,
    "Residual SE" = resid_SE,
    "Coefficient variance" = beta_var,
    "Coefficient standard deviation" = beta_SE,
    "t-values" = t_val,
    "p-values" = p_val,
    "Model matrix" = X,
    "Dependent variable" = Y,
    "formula" = formula,
    "dataname" = deparse(substitute(data)),
    "Square root of standardized residuals" = resid_standardized_sqrt
  )
  class(output) <- "linreg"
  return(output)
}