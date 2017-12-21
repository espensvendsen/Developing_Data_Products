# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Building a model with Top 10 feautures
#'
#' This function develops a prediction algorithm based on the top 10 features in 'x' that are most predictive of 'y'-
#'
#' @param x a n x p matrix of n observations and p predictors
#' @param y a vector of length n representing the response
#' @return a vector of coefficients from the final fitted model with top 10 features
#' @author Espen Svendsen
#' @details
#' This functions runs a univariate regression of y on each predictor in x and calculates
#' a p-value indicating the significance of the assosiaction. The final set of 10 predictors
#' is taken from the 10 smallest p-values.
#' @seealso \code{stats::lm}
#' @export
#' @importFrom("stats", "coef", "lm")
topten <- function(x, y) {
  p <- ncol(x)
  if (p < 10)
    stop("Number of preditors must be at least 10")
  pvalues <- numeric(p)
  for (i in seq_len(p)) {
    fit <- lm(y ~x[,i])
    summ <- summary(fit)
    pvalues[i] <- summ$coefficients[2, 4]
  }
  ord <- order(pvalues)
  ord <- ord[1:10]
  x10 <- x[, ord]
  fit <- lm (y ~ x10)
  coef(fit)
}

#' Prediction with the top 10 features
#'
#' This function takes a set of coefficients produced by the \code{topten} function
#' and makes a prediction for each of the values provided in the input 'X' matrix.
#'
#' @param X a n x 10 matrix containing n new observations
#' @param b a vector of coefficients obtained from the \code{topten} function
#' @return a numeric vector cointaing the predicted values
#' @export x
predict10 <- function(X, b) {
  X <- cbind(1, X)
  drop(X %*% b)

}
