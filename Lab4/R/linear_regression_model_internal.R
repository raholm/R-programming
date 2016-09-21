#' Internal Functions
#'
#' All the internal functions used in this package
#' (DO NOT EXPORT)
#'
#' @import ggplot2
#' @importFrom stats quantile sd
## Helper Functions ------------------------------------------------------------
.format_number <- function(number, decimals, ...) {
    formatted <- as.numeric(format(round(number, decimals), nsmall=decimals, ...))
    names(formatted) <- names(number)
    return(formatted)
}

.readkey <- function() {
    cat ("Press [enter] to continue")
    line <- readline()
    return(invisible())
}

## Initialization --------------------------------------------------------------
.initialize <- function(object, call, coefficients,
                        residuals, fitted.values,
                        df, ...) {
    ## Extract the string representation of the call
    object$call <- gsub(" +", " ", paste(deparse(call), collapse=""))

    object$coefficients <- coefficients
    object$residuals <- residuals
    object$fitted.values <- fitted.values
    object$df <- df

    return(invisible())
}

## Summary ---------------------------------------------------------------------
.summary <- function(object, ...) {
    .summary.call(object, ...)
    .summary.resid(object, ...)
    .summary.coef(object, ...)
    .summary.df(object, ...)
    return(invisible())
}

.summary.call <- function(object, ...) {
    cat("\nCall:\n")
    cat(object$call)
    cat("\n\n")
    return(invisible())
}

.summary.resid <- function(object, ...) {
    residuals <- object$residuals

    min <- .format_number(min(residuals$val), 4)
    quartile1 <- .format_number(quantile(residuals$val)[2], 4)
    median <- .format_number(median(residuals$val), 4)
    quartile3 <- .format_number(quantile(residuals$val)[4], 4)
    max <- .format_number(max(residuals$val), 4)

    names <- c("min", "1Q", "median", "3Q", "max")

    resid_statistics <- c(min, quartile1, median, quartile3, max)
    names(resid_statistics) <- names

    cat("Residuals:\n")
    base::print(resid_statistics)
    cat("\n")
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

.summary.df <- function(object, ...) {
    se <- .format_number(sqrt(object$residuals$var), 4)
    df <- object$df

    cat("\nResidual standard error: ")
    cat(se)
    cat(" on ")
    cat(df)
    cat(" degrees of freedom\n\n")
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

## Plot ------------------------------------------------------------------------
.plot <- function(object, ...) {
    .plot.resid_vs_fit(object, ...)

    ## Wait for user input before continuing
    .readkey()

    .plot.scale_location(object, ...)
    return(invisible())
}

.plot.resid_vs_fit <- function(object, ...) {
    label.title <- "Residuals vs Fitted"
    label.x <- paste("Fitted values", object$call, sep="\n")
    label.y <- "Residuals"

    data <- data.frame(x=object$fitted.values, y=object$residuals$val)

    res_vs_fit_plot <-  .plot.base(data, label.title, label.x, label.y) +
        geom_hline(yintercept=0, linetype="dotted", color="blue")

    suppressWarnings(base::print(res_vs_fit_plot))
    return(invisible())
}

.plot.scale_location <- function(object, ...) {
    label.title <- "Scale-Location"
    label.x <- paste("Fitted values", object$call, sep="\n")
    label.y <- expression(sqrt("Standardized residuals"))

    standardized_residuals <- abs(object$residuals$val / sd(object$residuals$val))
    data <- data.frame(x=object$fitted.values, y=sqrt(standardized_residuals))

    scale_location_plot <- .plot.base(data, label.title, label.x, label.y) +
        scale_y_continuous(limits=c(0, max(abs(data$y))))

    suppressWarnings(base::print(scale_location_plot))
    return(invisible())
}

.plot.base <- function(data, title, xlab, ylab) {
    outliers <- data[.outliers(data, 3), ]
    
    ## TODO: Is the smoothing method correct?
    return(ggplot() +
           ggtitle(title) +
           xlab(xlab) +
           ylab(ylab) +
           theme(plot.title=element_text(hjust=0.5)) +
           theme_bw()+
           theme(plot.background = element_rect(fill = "#54D8E0"))+
           geom_point(data=data, aes_string(x="x", y="y")) +
           geom_smooth(data=data, aes_string(x="x", y="y"), method="loess",
                       color="red", se=FALSE) +
           geom_text(data=outliers, aes_string(x="x", y="y", label="rownames(outliers)"),
                     hjust=0, nudge_x = 0.05))
}

.outliers <- function(data, count) {
    return(order(abs(data$y), decreasing=TRUE)[1:count])
}
