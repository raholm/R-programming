#' A Reference Class to represent linear regression models.
#'
#' Stores information about a linear regression model generated by \code{linreg}.
#'
#' @field call Object of class \code{"character"}. A string representation of the calling code.
#' @field coefficients Object of class \code{"list"}. A list containing information about the estimated coefficients.
#' \describe{
#' \item{\code{val}:}{The estimated coefficients.}
#' \item{\code{se}:}{The standard errors of the coefficients.}
#' \item{\code{var}:}{The variance of the coefficients.}
#' \item{\code{tval}:}{The t values of the the coefficients.}
#' \item{\code{pval}:}{The p values of the coefficients.}
#' }
#' @field residuals Ojbect of class \code{"list"}. A list containing information about the residuals.
#' \describe{
#' \item{\code{val}:}{The residuals.}
#' \item{\code{var}:}{The variance of the residuals.}
#' }
#' @field fitted.values  The fitted values of y using the model's coefficients.
#' @field df  The number of degrees of freedom in the model.
#'
#' @import methods
#'
#' @aliases linregmod
#' @exportClass LinearRegressionModel
LinearRegressionModel <- setRefClass("LinearRegressionModel",
                                     fields=list(
                                         call="character",
                                         coefficients="list",
                                         residuals="list",
                                         fitted.values="numeric",
                                         df="numeric"
                                     ))

## Constructor and Destructor
LinearRegressionModel$methods(list(
                          initialize = function(call, coefficients, residuals, fitted.values,
                                                df, ...) {
                              .initialize(.self, call, coefficients, residuals, fitted.values, df, ...)
                              callSuper(...)
                          }
                      ))

## Methods
LinearRegressionModel$methods(list(
                          coef = function() {
                              "Returns the estimated coefficients."
                              return(coefficients$val)
                          },
                          resid = function() {
                              "Returns the residuals."
                              return(residuals$val)
                          },
                          pred = function() {
                              "Returns the fitted values."
                              return(fitted.values)
                          },
                          summary = function() {
                              "Shows a summary of the model."
                              .summary(.self)
                          },
                          print = function() {
                              "Prints the model."
                              .print(.self)
                          },
                          show = function() {
                              print()
                          },
                          plot = function() {
                              "Plots Residuals vs Fitted and Scale-Location."
                              .plot(.self)
                          }
                      ))
