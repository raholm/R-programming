#' Linear Regression
#'
#' linreg is used to fit linear regression models.
#'
#' @usage
#' linreg(formula, data)
#'
#' @param formula an object of class \code{formula}. Describes the model to be fitted.
#' @param data a data frame containing the variables in the model.
#' @return The fitted model of class \code{"LinearRegressionModel"}.
#'
#' @examples
#' linreg(formula = Petal.Width ~ Petal.Length, data=iris)
#' linreg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
#' linreg(Petal.Width ~ Species, iris)
#'
#' @importFrom stats model.matrix
#' @importFrom stats pt
#'
#' @export
#' @source \url{https://en.wikipedia.org/wiki/Linear_regression}

ridgereg <- function(formula, data, lambda){
  call<- match.call()
  
  X<- model.matrix(formula,data)
  #####sd() 
  Xnorm <- (X - mean(X))/ sd(X)
  
  y <- as.matrix(data[,all.vars(formula)[1]])
  
  reg.coe <- solve(t(X) %*% X - lambda #####*I
  ) %*% t(X) %*% y
  
  fit.val <- X %*% reg.coe
  
  return(ridgeregclass(
    call = as.character(call),
    coefficients = t(reg.coe),
    fitted.values = as.numeric(fit.val)
  ))
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

linreg_check_input <- function(formula, data)
{
    stopifnot(length(all.vars(formula)) > 0)
    stopifnot(is.data.frame(data))

    ## Ensures that all variables in the formula
    ## are present in the data frame
    variables <- all.vars(formula)
    for (variable in variables)
    {
        stopifnot(variable %in% names(data))
    }
}

linreg_X <- function(formula, data) {
    return(model.matrix(object=formula, data=data))
}

linreg_y <- function(formula, data, X) {
    ## Limit the use to only one dependent variable
    ## TODO: Fix this limitation
    y_variables <- all.vars(formula)[1]
    y <- data[, y_variables]
    return(as.matrix(y))
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

linreg_residuals <- function(y, fitted_values) {
    residuals <- y - fitted_values
    residuals <- as.vector(residuals)
    names(residuals) <- 1:length(residuals)
    return(residuals)
}

linreg_residuals_variance <- function(residuals, df) {
    residuals_variance <- (t(residuals) %*% residuals) / df
    return(as.numeric(residuals_variance))
}

linreg_inverse_QR_decomposition <- function(X) {
    return(qr.solve(qr(X)))
}
