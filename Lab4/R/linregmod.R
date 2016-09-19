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
#' @name linregmod
#' @source \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html}
.linreg <- setRefClass("LinearRegressionModel",
                       fields=list(
                           call="call",
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
                call <<- call
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
                callstr <- function() {
                       return(gsub(" +", " ", paste(deparse(call), collapse="")))
                }

                format_number <- function(number, decimals) {
                    formatted <- as.numeric(format(round(number, decimals), nsmall=decimals))
                    names(formatted) <- names(number)
                    return(formatted)
                }

                cat("\nCall:\n")
                cat(callstr())
                cat("\n\n")
                cat("Coefficients:\n  ")
                base::print(format_number(coef(), 4))
            },
            show = function() {
                print()
            },
            plot = function() {
            }
        ))
