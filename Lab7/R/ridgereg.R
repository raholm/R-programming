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
  
  X <- ridge_reg(formula, data)
  y <- ridge_reg(formula, data)
  
  Xnorm <- (X - mean(X))/ sd(X)
  
  coefficients <- ridgereg_coefficients(Xnorm, y, lambda)
  fitted_values <- ridgereg_fitted_values(Xnorm, coefficients)
  
  return(ridgeregmod(
      call = call,
      coefficients = coefficients,
      fitted.values = fitted_values)
  ))
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

ridgereg_coefficients <- function(X, y, lambda) {
    coefficients <- solve(t(X) %*% X - lambda * diag(dim(X)[2])) %*% t(X) %*% y
    names(coefficients) <- colnames(X)
    return(coefficients)
}

ridgereg_fitted_values <- function(X, coefficients) {
    fitted_values <- X %*% coefficients
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
}


linreg <- function(formula, data)
{
    linreg_check_input(formula, data)

    call <- match.call()

    X <- linreg_X(formula, data)
    y <- linreg_y(formula, data, X)

    df <- linreg_df(X)

    coefficients <- list()
    coefficients$val <- linreg_coefficients(X, y)
    fitted_values <- linreg_fitted_values(X, coefficients$val)

    residuals <- list()
    residuals$val <- linreg_residuals(y, fitted_values)
    residuals$var <- linreg_residuals_variance(residuals$val, df)

    coefficients$var <- linreg_coefficients_variance(X, residuals$var)
    coefficients$se <- linreg_coefficients_standard_error(coefficients$var)
    coefficients$tval <- linreg_coefficients_t_value(coefficients$val, coefficients$var)
    coefficients$pval <- linreg_coefficients_p_value(coefficients$tval, df)

    return(LinearRegressionModel(call=call,
                                 coefficients=coefficients,
                                 fitted.values=fitted_values,
                                 residuals=residuals,
                                 df=df))
}


linreg_df <- function(X) {
    return(nrow(X) - ncol(X))
}

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

linreg_coefficients_variance <- function(X, residuals_variance) {
    coefficients_variance <- residuals_variance * linreg_inverse_QR_decomposition(t(X) %*% X)
    coefficients_variance <- as.vector(diag(coefficients_variance))
    names(coefficients_variance) <- colnames(X)
    return(coefficients_variance)
}

linreg_coefficients_standard_error <- function(coefficients_variance) {
    return(sqrt(coefficients_variance))
}

linreg_coefficients_t_value <- function(coefficients, coefficients_variance) {
    coefficients_t_value <- coefficients / sqrt(coefficients_variance)
    return(coefficients_t_value)
}

linreg_coefficients_p_value <- function(t_values, df) {
    p_value <- 2 * pt(-abs(t_values), df)
    return(p_value)
}

linreg_fitted_values <- function(X, coefficients) {
    fitted_values <- X %*% coefficients
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
}

linreg_inverse_QR_decomposition <- function(X) {
    return(qr.solve(qr(X)))
}
