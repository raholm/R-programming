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
#' @source \url{https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html}

.linreg <- setRefClass("LinearRegression",
                       fields=list(
                           coefficients="vector",
                           residuals="vector",
                           fitted.values="vector"
                           ## df="numeric",
                           ## residuals.variance="numeric"
                           ## coefficients.variance="numeric",
                       ))


## Constructor and Destructor
.linreg$methods(list(
            initialize = function(coefficients, residuals, fitted.values, ...) {
                cat("Initialize: Linear Regression\n")

                coefficients <<- coefficients
                residuals <<- residuals
                fitted.values <<- fitted.values

                callSuper(...)
            },
            finalize = function() {
                cat("Finalize: Linear Regression\n")
            }
        ))

## Generic Functions
.linreg$methods(list(
            coef = function() {
                cat("Coef: Linear Regression\n")
                return(coefficients)
            },
            resid = function() {
                cat("Resid: Linear Regression\n")
                return(residuals)
            },
            pred = function() {
                cat("Pred: Linear Regression\n")
                return(fitted.values)
            },
            summary = function() {
                cat("Summary: Linear Regression\n")
            },
            print = function() {
                cat("Print: Linear Regression\n")
            },
            plot = function() {
                cat("Plot: Linear Regression\n")
            }
        ))

## Template
## setGeneric("print", function(object, ...) standardGeneric("print"))
## setMethod(print, signature(object="LinearRegression"),
##           function(object, ...) {
##               object$print()
##           })
