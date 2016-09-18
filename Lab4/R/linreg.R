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

    coefficients <- linreg_coefficients(X, y)
    fitted_values <- linreg_fitted_values(X, coefficients)
    residuals <- linreg_residuals(y, fitted_values)

    return(.linreg(call=call,
                   coefficients=coefficients,
                   fitted.values=fitted_values,
                   residuals=residuals))
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

linreg_coefficients <- function(X, y) {
    ## TODO: Rewrite using QR decomposition
    coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
    coefficients <- as.vector(coefficients)
    names(coefficients) <- colnames(X)
    return(coefficients)
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
