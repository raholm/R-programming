#' Ridge Regression
#'
#' ridgereg is used to fit ridge regression models.
#'
#' @usage
#' ridgereg(formula, data, lambda=0)
#'
#' @param formula an object of class \code{formula}. Describes the model to be fitted.
#' @param data a data frame containing the variables in the model.
#' @param lambda A constant >= 0
#' @return The fitted model of class \code{"LinearRegressionModel"}.
#'
#' @examples
#' ridgereg(formula = Petal.Width ~ Petal.Length, data=iris)
#' ridgereg(formula = Petal.Width ~ Petal.Length, data=iris, lambda=2)
#'
#' @importFrom stats model.matrix
#' @importFrom stats pt
#'
#' @export
#' @source \url{https://en.wikipedia.org/wiki/Tikhonov_regularization}
ridgereg <- function(formula, data, lambda=0){
  ridgereg_check_input(formula, data, lambda)

  call <- match.call()
  
  return(RidgeRegressionModel(call=call,
                              formula=formula,
                              data=data,
                              lambda=lambda))
}

ridgereg_check_input <- function(formula, data, lambda)
{
    stopifnot(length(all.vars(formula)) > 0)
    stopifnot(is.data.frame(data))
    stopifnot(is.numeric(lambda) && lambda >= 0)

    ## Ensures that all variables in the formula
    ## are present in the data frame
    variables <- all.vars(formula)
    for (variable in variables)
    {
        stopifnot(variable %in% names(data))
    }
}

## REMOVE BELOW WHEN DONE ----------------------------------------

linreg_coefficients <- function(X, y) {
    ## Source: http://www.stats.ox.ac.uk/~konis/Rcourse/qr.pdf
    qr.X <- qr(X)
    b <- t(qr.Q(qr.X)) %*% y
    R <- qr.R(qr.X)
    coefficients <- backsolve(R, b)
    coefficients <- as.vector(coefficients)
    names(coefficients) <- colnames(X)
    return(coefficients)
}
