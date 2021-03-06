#' Internal Functions
#'
#' All the internal functions of the LinearRegressionModel class.
#' (DO NOT EXPORT)
#'
#' @import ggplot2
#' @importFrom stats quantile sd
#' @importFrom png readPNG
#' @importFrom grid rasterGrob
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
    plot1 <- .plot.resid_vs_fit(object, ...)
    plot2 <- .plot.scale_location(object, ...)

    ## TODO: Very ugly but I tried!!!
    img <- readPNG(system.file("img", "LiU_logo.png", package="Lab4"), native=TRUE)
    grob <- rasterGrob(img, interpolate=TRUE)

    ## TODO: Very ugly but I tried!!!
    yranges <- ggplot_build(plot1)$panel$ranges[[1]]$y.range
    xranges <- ggplot_build(plot1)$panel$ranges[[1]]$x.range
    plot1 <- plot1 + annotation_custom(grob, xmin=xranges[1], xmax=xranges[1] + (xranges[2] / 5),
                                       ymin=yranges[1], ymax=yranges[1] + (yranges[2] / 5))

    suppressWarnings(base::print(plot1))

    ## ## Wait for user input before continuing
    .readkey()

    ## TODO: Very ugly but I tried!!!
    yranges <- ggplot_build(plot2)$panel$ranges[[1]]$y.range
    xranges <- ggplot_build(plot2)$panel$ranges[[1]]$x.range
    plot2 <- plot2 + annotation_custom(grob, xmin=xranges[1], xmax=xranges[1] + (xranges[2] / 5),
                                       ymin=yranges[1], ymax=yranges[1] + (yranges[2] / 5))

    suppressWarnings(base::print(plot2))

    return(invisible())
}

.plot.resid_vs_fit <- function(object, ...) {
    label.title <- "Residuals vs Fitted"
    label.x <- paste("Fitted values", object$call, sep="\n")
    label.y <- "Residuals"

    data <- data.frame(x=object$fitted.values, y=object$residuals$val)

    res_vs_fit_plot <-  .plot.base(data, label.title, label.x, label.y) +
        geom_hline(yintercept=0, linetype="dotted", color="blue")
    return(res_vs_fit_plot)
}

.plot.scale_location <- function(object, ...) {
    label.title <- "Scale-Location"
    label.x <- paste("Fitted values", object$call, sep="\n")
    label.y <- expression(sqrt("Standardized residuals"))

    standardized_residuals <- abs(object$residuals$val / sd(object$residuals$val))
    data <- data.frame(x=object$fitted.values, y=sqrt(standardized_residuals))

    scale_location_plot <- .plot.base(data, label.title, label.x, label.y) +
        scale_y_continuous(limits=c(0, max(abs(data$y))))
    return(scale_location_plot)
}

.plot.base <- function(data, title, xlab, ylab) {
    outliers <- .outliers(data, 3)

    return(ggplot(data=data) +
           ggtitle(title) +
           xlab(xlab) +
           ylab(ylab) +
           theme_bw() +
           theme(plot.title=element_text(hjust=0.5),
                 plot.background = element_rect(fill = "#54D8E0"),
                 panel.grid.minor = element_blank()) +
           geom_point(aes_string(x="x", y="y")) +
           geom_smooth(aes_string(x="x", y="y"), method="loess",
                       color="red", se=FALSE) +
           geom_text(data=outliers, aes_string(x="x", y="y", label="rownames(outliers)"),
                     hjust=0, nudge_x = 0.05, check_overlap=TRUE))
}

.outliers <- function(data, count) {
    return(data[order(abs(data$y), decreasing=TRUE)[1:count], ])
}
