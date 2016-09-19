#' title here
#'
#' description here
#'
#' @param param description
#' @return
#'
#' @examples
#' examples here
#'
#' @export
#' @source \url{}
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
    coefficients$se <- linreg_coefficients_standard_error(X, residuals$var)
    coefficients$tval <- linreg_coefficients_t_value()
    coefficients$pval <- linreg_coefficients_p_value()

    return(.linreg(call=call,
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
    y_variables <- all.vars(formula)[!(all.vars(formula) %in% colnames(X))]
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
    beta <- backsolve(R, b)
    beta <- as.vector(beta)
    names(beta) <- colnames(X)
    return(beta)
}

linreg_coefficients_variance <- function(X, residuals_variance) {
    ## Note: Use QR decomposition
    ## Source: http://www.stats.ox.ac.uk/~konis/Rcourse/qr.pdf
    coefficients_variance <- residuals_variance * solve(t(X) %*% X)
    return(coefficients_variance)
}

linreg_coefficients_standard_error <- function(X, residuals_variance) {
    variance_covariance_matrix <- residuals_variance * solve(t(X) %*% X)
    standard_error <- sqrt(diag(variance_covariance_matrix))
    names(standard_error) <- colnames(X)
    return(standard_error)
}

linreg_coefficients_t_value <- function(coefficients) {

}

linreg_coefficients_p_value <- function(...) {

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
