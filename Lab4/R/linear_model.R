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
                               coefficients.variance="numeric",
                               residuals="vector",
                               fitted.values="vector",
                               df="numeric",
                               residuals.variance="numeric"
                           ))


## Constructor and Destructor
.linreg$methods(list(
            initialize = function(coefficients, ...) {
                cat("Initialize: Linear Regression\n")

                coefficients <<- coefficients

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
            },
            resid = function() {
                cat("Resid: Linear Regression\n")
            },
            pred = function() {
                cat("Pred: Linear Regression\n")
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
setGeneric("print", function(object, ...) standardGeneric("print"))
setMethod(print, signature(object="LinearRegression"),
          function(object, ...) {
              object$print()
          })



## test <- .linreg(c(1, 2, 3))
## print(test)
## test$coefficients

## data(iris)
## model <- lm(Petal.Width ~ Petal.Length, data=iris)
## model
## model$coefficients
## model$residuals
## model$fitted.value
## predict(model)
## summary(model)
