#' Internal Functions
#'
#' All the internal functions of the RidgeRegressionModel class.
#' (DO NOT EXPORT)
#'
#' @importFrom stats sd as.formula
## Helper Functions ------------------------------------------------------------
.format_number <- function(number, decimals, ...) {
    formatted <- as.numeric(format(round(number, decimals), nsmall=decimals, ...))
    names(formatted) <- names(number)
    return(formatted)
}

## Initialization --------------------------------------------------------------
.initialize <- function(object, call, formula, data, lambda, ...) {
    ## Extract the string representation of the call
    object$call <- gsub(" +", " ", paste(deparse(call), collapse=""))

    X <- .X(formula, data)
    y <- .y(formula, data)

    Xnorm <- .standardise(object, X)

    object$variables <- colnames(X)[-1]
    object$lambda <- lambda
    object$coefficients <- .QR_coefficients(Xnorm, y, lambda)
    object$fitted.values <- .fitted_values(object, Xnorm)

    return(invisible())
}

.X <- function(formula, data) {
    return(model.matrix(object=formula, data=data))
}

.standardise <- function(object, X) {
    intercept <- .has_intercept(X)

    if (intercept) {
        X <- X[, -which(colnames(X) == "(Intercept)")]
    }

    object$center <- colMeans(X)
    object$scale <- apply(X, 2, sd)

    ## Could use the scale() method which is probably a much better choice
    ## but we are here to learn so lets do it the manual (bad) way.
    center_matrix <- matrix(rep(object$center, nrow(X)), ncol=length(object$center), byrow=TRUE)
    scale_matrix <- matrix(rep(object$scale, nrow(X)), ncol=length(object$scale), byrow=TRUE)

    X <- (X - center_matrix) / scale_matrix

    X <- cbind(rep(1, nrow(X)), X)
    colnames(X)[1] <- "(Intercept)"

    return(X)
}

.has_intercept <- function(X) {
    return("(Intercept)" %in% colnames(X))
}

.y <- function(formula, data) {
    y_variables <- all.vars(formula)[1]
    y <- data[, y_variables]
    return(as.matrix(y))
}

.normal_coefficients <- function(X, y, lambda) {
    lambda_matrix <- lambda * diag(ncol(X))
    ## lambda_matrix <- t(lambda_matrix) %*% lambda_matrix

    coefficients <- solve(t(X) %*% X + lambda_matrix) %*% t(X) %*% y
    coefficients <- as.vector(coefficients)

    ## Should I rescale the coefficients here and use those
    ## at all times or scale it according to the data that
    ## is being predicted?
    ## coefficients[-1] <- coefficients[-1] / scale

    ## Remove intercept name
    names(coefficients) <- c("", colnames(X)[-1])
    return(coefficients)
}

.QR_coefficients <- function(X, y, lambda) {
    ## Source: https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
    A <- X
    b <- y
    tau <- lambda * diag(ncol(X))

    B <- rbind(A, tau)
    Bx <- c(b, rep(0, nrow(tau)))

    qr.B <- qr(B)
    b <- t(qr.Q(qr.B)) %*% Bx
    R <- qr.R(qr.B)
    coefficients <- backsolve(R, b)
    coefficients <- as.vector(coefficients)
    names(coefficients) <- colnames(X)
    return(coefficients)
}

## Coef ------------------------------------------------------------------------
.coef <- function(object, ...) {
    return(object$coefficients)
}

## Pred ------------------------------------------------------------------------
.pred <- function(object, X, ...) {
    if (is.null(X)) {
        return(object$fitted.values)
    }

    .pred.check_input(object, X)

    X <- .filter_variables(object, X)

    ## Not sure if the data should be normalized
    ## before the prediction or just when actually
    ## getting the coefficients and then rescale
    ## the coefficients and use those without any
    ## further scaling of input data
    Xnorm <- .standardise(object, X)

    return(.fitted_values(object, Xnorm))
}

.pred.check_input <- function(object, X) {
    stopifnot(is.matrix(X))

    for (variable in object$variables) {
        stopifnot(variable %in% colnames(X))
    }
}

.filter_variables <- function(object, X) {
    return(X[, object$variables])
}

.fitted_values <- function(object, X) {
    fitted_values <- X %*% (object$coef() / c(1, object$scale))
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
}

## Print -----------------------------------------------------------------------
.print <- function(object, ...) {
    ## Might wanna use strwrap.
    cat("\nCall:\n")
    cat(object$call)
    cat("\n\n")
    cat("Coefficients:\n  ")
    coefficients <- object$coef()
    names(coefficients)[1] <- "(Intercept)"
    base::print(.format_number(coefficients, 4))
    cat("\n")
    return(invisible())
}
