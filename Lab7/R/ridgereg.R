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
#' @source \url{https://en.wikipedia.org/wiki/Linear_regression}
ridgereg <- function(formula, data, lambda=0){
  ridgereg_check_input(formula, data, lambda)

  call <- match.call()
  
  X <- ridgereg_X(formula, data)
  y <- ridgereg_y(formula, data)

  Xmean <- colMeans(X[, -1])
  Xscale <- apply(X[, -1], 2, sd)

  center_matrix <- matrix(rep(Xmean, nrow(X)), ncol=length(Xmean), byrow=TRUE)
  scale_matrix <- matrix(rep(Xscale, nrow(X)), ncol=length(Xscale), byrow=TRUE)
  Xnorm <- cbind(rep(1, nrow(X)), (X[, -1] - center_matrix) / scale_matrix)

  coefficients <- list()
  coefficients$val <- ridgereg_coefficients(Xnorm, y, lambda, Xscale)
  ## coefficients$val[1] <- (coefficients$val[1] - mean(Xmean)) / mean(Xscale)

  fitted_values <- ridgereg_fitted_values(Xnorm, coefficients$val)

  return(RidgeRegressionModel(call=call,
                              coefficients=coefficients,
                              fitted.values=fitted_values,
                              center=Xmean, scale=Xscale,
                              variables=colnames(X)[-1]))
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

ridgereg_X <- function(formula, data) {
    return(model.matrix(object=formula, data=data))
}

ridgereg_y <- function(formula, data) {
    y_variables <- all.vars(formula)[1]
    y <- data[, y_variables]
    return(as.matrix(y))
}

ridgereg_coefficients <- function(X, y, lambda, scale) {
    lambda_matrix <- lambda * diag(ncol(X))
    ## lambda_matrix <- t(lambda_matrix) %*% lambda_matrix

    coefficients <- solve(t(X) %*% X + lambda_matrix) %*% t(X) %*% y
    coefficients <- as.vector(coefficients)
    coefficients[-1] <- coefficients[-1] / scale
    # Remove intercept name
    names(coefficients) <- c("", colnames(X)[-1])
    return(coefficients)
}

ridgereg_fitted_values <- function(X, coefficients) {
    fitted_values <- X %*% coefficients
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
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
