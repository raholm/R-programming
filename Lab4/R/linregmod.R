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
                           coefficients="vector",
                           residuals="vector",
                           fitted.values="vector"
                           ## df="numeric",
                           ## residuals.variance="numeric"
                           ## coefficients.variance="numeric",
                       ))


## Constructor and Destructor
.linreg$methods(list(
            initialize = function(call, coefficients, residuals, fitted.values, ...) {
                cat("Initialize: Linear Regression Model\n")

                call <<- call
                coefficients <<- coefficients
                residuals <<- residuals
                fitted.values <<- fitted.values

                callSuper(...)
            },
            finalize = function() {
                cat("Finalize: Linear Regression Model\n")
            }
        ))

## Generic Functions
.linreg$methods(list(
            coef = function() {
                cat("Coef: Linear Regression Model\n")
                return(coefficients)
            },
            resid = function() {
                cat("Resid: Linear Regression Model\n")
                return(residuals)
            },
            pred = function() {
                cat("Pred: Linear Regression Model\n")
                return(fitted.values)
            },
            summary = function() {
                cat("Summary: Linear Regression Model\n")
            },
            print = function() {
                cat("Print: Linear Regression Model\n")
            },
            plot = function() {
                cat("Plot: Linear Regression Model\n")
            }
        ))

## Template
## setGeneric("print", function(object, ...) standardGeneric("print"))
## setMethod(print, signature(object="LinearRegression"),
##           function(object, ...) {
##               object$print()
##           })
