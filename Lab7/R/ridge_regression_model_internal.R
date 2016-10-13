#' Internal Functions
#'
#' All the internal functions of the RidgeRegressionModel class.
#' (DO NOT EXPORT)
#'
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
    object$coefficients <- .normal_coefficients(Xnorm, y, lambda)
    object$fitted.values <- .fitted_values(object, Xnorm)

    return(invisible())
}

.X <- function(formula, data) {
    return(model.matrix(object=formula, data=data))
}

.standardise <- function(object, X) {
    intercept <- .has_intercept(X)

    object$center <- colMeans(X[, -intercept])
    object$scale <- apply(X[, -intercept], 2, sd)

    center_matrix <- matrix(rep(object$center, nrow(X)), ncol=length(object$center), byrow=TRUE)
    scale_matrix <- matrix(rep(object$scale, nrow(X)), ncol=length(object$scale), byrow=TRUE)

    X[, -intercept] <- (X[, -intercept] - center_matrix) / scale_matrix

    if (!intercept) {
        X <- cbind(rep(1, nrow(X)), X)
    }

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
    ## coefficients[-1] <- coefficients[-1] / scale
    ## Remove intercept name
    names(coefficients) <- c("", colnames(X)[-1])
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

    Xnorm <- .standardise(object, X)

    return(.fitted_values(object, Xnorm))
}

.pred.check_input <- function(object, X) {
    stopifnot(is.matrix(X))

    for (variable in object$variables) {
        stopifnot(variable %in% colnames(X))
    }
}

.fitted_values <- function(object, X) {
    fitted_values <- X %*% object$coef() / object$scale
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
}


## Summary ---------------------------------------------------------------------
.summary <- function(object, ...) {
    .summary.call(object, ...)
    .summary.coef(object, ...)
    return(invisible())
}

.summary.call <- function(object, ...) {
    cat("\nCall:\n")
    cat(object$call)
    cat("\n\n")
    return(invisible())
}

.summary.coef <- function(object, ...) {
    coefficients <- object$coefficients

    rownames <- names(coefficients)
    coef_statistics <- matrix(c(.format_number(coefficients, 4)),
                              byrow=FALSE,
                              nrow=length(coefficients),
                              row.names=rownames)

    cat("Coefficients:\n")
    base::print(coef_statistics)
    cat("\n")
    return(invisible())
}

## Print -----------------------------------------------------------------------
.print <- function(object, ...) {
    ## Might wanna use strwrap.
    cat("\nCall:\n")
    cat(object$call)
    cat("\n\n")
    cat("Coefficients:\n  ")
    base::print(.format_number(object$coef(), 4))
    cat("\n")
    return(invisible())
}
