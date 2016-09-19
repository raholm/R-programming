#' Linear Regression Model
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
#' @name linregmod
#' @source \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html}
.linreg <- setRefClass("LinearRegressionModel",
                       fields=list(
                           call="character",
                           coefficients="list",
                           residuals="list",
                           fitted.values="vector",
                           df="numeric"
                           ## residuals.variance="numeric"
                           ## coefficients.variance="numeric",
                       ))


## Constructor and Destructor
.linreg$methods(list(
            initialize = function(call, coefficients, residuals, fitted.values,
                                  df, ...) {
                ## Extract the string representation of the call
                call <<- gsub(" +", " ", paste(deparse(call), collapse=""))

                coefficients <<- coefficients
                residuals <<- residuals
                fitted.values <<- fitted.values
                df <<- df

                callSuper(...)
            },
            finalize = function() {

            }
        ))

## Generic Functions
.linreg$methods(list(
            coef = function() {
                return(coefficients$val)
            },
            resid = function() {
                return(residuals$val)
            },
            pred = function() {
                return(fitted.values)
            },
            summary = function() {
            },
            print = function() {
                ## Might wanna use strwrap.
                format_number <- function(number, decimals) {
                    formatted <- as.numeric(format(round(number, decimals), nsmall=decimals))
                    names(formatted) <- names(number)
                    return(formatted)
                }

                cat("\nCall:\n")
                cat(call)
                cat("\n\n")
                cat("Coefficients:\n  ")
                base::print(format_number(coef(), 4))
            },
            show = function() {
                print()
            },
            plot = function() {
                readkey <- function() {
                    cat ("Press [enter] to continue")
                    line <- readline()
                }

                base_plot <- function(data, title, xlab, ylab) {
                    return(ggplot(data=data) +
                           ggtitle(title) +
                           xlab(xlab) +
                           ylab(ylab) +
                           theme(plot.title=element_text(hjust=0.5)) +
                           geom_point(aes(x=x, y=y)))
                }

                ## Residuals vs Fitted Plot -------------------
                label.title <- "Residuals vs Fitted"
                label.x <- paste("Fitted values", call, sep="\n")
                label.y <- "Residuals"

                data <- data.frame(x=fitted.values, y=residuals$val)

                res_vs_fit_plot <-  base_plot(data, label.title, label.x, label.y) +
                    geom_hline(yintercept=0, linetype="dotted", color="blue")

                base::print(res_vs_fit_plot)

                ## Wait for user input before continuing
                readkey()

                ## Scale-Location Plot ----------------------
                label.title <- "Scale-Location"
                label.y <- expression(sqrt("Standardized residuals"))

                standardized_residuals <- abs(residuals$val / sd(residuals$val))
                data <- data.frame(x=fitted.values, y=sqrt(standardized_residuals))

                scale_location_plot <- base_plot(data, label.title, label.x, label.y)
                base::print(scale_location_plot)
            }
        ))
