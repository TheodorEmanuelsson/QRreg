% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuals.R
\name{residuals.linreg}
\alias{residuals.linreg}
\title{Get residuals from a linreg object}
\usage{
\method{residuals}{linreg}(object, ...)
}
\arguments{
\item{object}{An object of class linreg}

\item{...}{Additional arguments that we don't use}
}
\description{
Issue with the resid function: Does not work as resid.linreg for some odd reason. Must mask from stats package
}
\examples{
fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
resid(fit)
}
