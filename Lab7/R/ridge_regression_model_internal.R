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
.initialize <- function(object, call, coefficients, fitted.values, center, scale, variables, ...) {
    ## Extract the string representation of the call
    object$call <- gsub(" +", " ", paste(deparse(call), collapse=""))

    object$coefficients <- coefficients
    object$fitted.values <- fitted.values
    object$center <- center
    object$scale <- scale
    object$variables <- variables

    return(invisible())
}

## Coef ------------------------------------------------------------------------
.coef <- function(object, ...) {
    ## coefficients <- object$coefficients$val
    ## coefficients[-1] <- coefficients[-1] / object$scale
    return(object$coefficients$val)
}

## Pred ------------------------------------------------------------------------
.pred <- function(object, X, ...) {
    if (is.null(X)) {
        return(object$fitted.values)
    }

    .pred.check_input(object, X)

    return(.fitted_values(object, X))
}

.pred.check_input <- function(object, X) {
    stopifnot(is.matrix(X))

    for (variable in object$variables) {
        stopifnot(variable %in% colnames(X))
    }
}

.fitted_values <- function(object, X) {
    center_matrix <- matrix(rep(object$center, nrow(X)), ncol=length(object$center), byrow=TRUE)
    scale_matrix <- matrix(rep(object$scale, nrow(X)), ncol=length(object$scale), byrow=TRUE)

    Xnorm <- (X - center_matrix) / scale_matrix

    ## Add intercept term if not present
    if (!("(Intercept)" %in% colnames(Xnorm))) {
        Xnorm <- cbind(rep(1, nrow(Xnorm)), Xnorm)
    }

    fitted_values <- Xnorm %*% object$coef()
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

    rownames <- names(coefficients$val)
    colnames <- c("Estimate", "Std. Error", "t value", "p value")
    coef_statistics <- matrix(c(.format_number(coefficients$val, 4),
                                .format_number(coefficients$se, 4),
                                .format_number(coefficients$tval, 4),
                                coefficients$pval),
                              byrow=FALSE,
                              nrow=length(coefficients$val),
                              dimnames=list(rownames, colnames))

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
