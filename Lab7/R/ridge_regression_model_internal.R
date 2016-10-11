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
.initialize <- function(object, call, coefficients,
                        fitted.values, ...) {
    ## Extract the string representation of the call
    object$call <- gsub(" +", " ", paste(deparse(call), collapse=""))

    object$coefficients <- coefficients
    object$fitted.values <- fitted.values

    return(invisible())
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
