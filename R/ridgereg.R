#' Compute Ridge Regression
#'
#' @param formula A model formula
#' @param data A dataframe
#' @param lambda A numeric scalar
#' @param use_QR Boolean for if QR-decomposition should be used
#' @return A ridgereg object
#' @details This function computes ridge regression using least squares or QR-decomposition.
#' @examples
#' fit <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 2)
#' fit2 <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 2, use_QR = TRUE)
#' @importFrom stats model.matrix pt
#' @export 
#' 

ridgereg <- function(formula, data, lambda, use_QR = FALSE) {
  if (class(formula) != "formula") {
    stop("Error: Input not valid formula")
  }
  if (is.data.frame(data) != TRUE){
    stop("Error: Invalid data")
  }
  stopifnot(is.numeric(lambda) == TRUE, length(lambda) == 1, is.logical(use_QR))
  
  ## Model matrix and dependent variable
  X <- model.matrix(formula, data)
  Y_name <- all.vars(formula)[1]
  Y <- as.matrix(data[Y_name])
  # Normalize by mean and sd
  X[,2:3] <- scale(X[,2:3])
  # Identity matrix
  I <- diag(lambda, nrow = ncol(X), ncol = ncol(X))
  
  if(use_QR == TRUE){
    QR <- qr(X) 
    R <- qr.R(QR)
    Q <- qr.Q(QR)
    QtQ <- t(Q) %*% Q
    invRt <- solve(t(R)) # Find inverse of transpose of R
    XtY <- t(Q) %*% Y # Cross product of Q and Y
    R_R <- ((R %*% QtQ) + lambda * (QtQ %*% invRt)) 
    beta_hat <- backsolve(R_R, XtY)
    # coefs
    #beta_hat <- solve(R + I) %*% Qt_Y #DEPRECATED
  } else{
    beta_hat <- solve(((t(X)%*%X)+I)) %*% (t(X)%*%Y)
  }
  
  names(beta_hat) <- colnames(X)
  names(beta_hat)[1] <- ""
  fit_val <- as.numeric(X %*% beta_hat)
  
  output <- list(
    "Coefficients" = beta_hat,
    "Fitted values" = fit_val,
    "Model matrix" = X,
    "Dependent variable" = Y,
    "formula" = formula,
    "dataname" = deparse(substitute(data)),
    "lambda" = lambda,
    "QR" = use_QR)
  
  class(output) <- "ridgereg"
  return(output)
}

