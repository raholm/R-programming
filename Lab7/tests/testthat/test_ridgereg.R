library(Lab7)
library(MASS)
context("ridgereg")

test_that("ridgereg of invalid input is invalid", {
    expect_error(ridgereg(1, iris))
    expect_error(ridgereg(y ~ x, 1))
    # The variables x, y does not exist in iris.
    expect_error(ridgereg(y ~ x, iris))
    expect_error(ridgereg(formula=y ~ x))
    expect_error(ridgereg(data=iris))
})

test_that("ridgereg of valid input is correct", {
    check_model_methods <- function(actual, expected, data) {
        scaled_data <- scale(data, center = model.expected$xm, scale = model.expected$scales)
        expect_equal(actual$coefficients, expected$coef)
        expect_equal(actual$fitted.values, scaled_data %*% model.expected$coef)
    }

    ## Simple Model
    ## model.expected <- lm.ridge(Petal.Width ~ Petal.Length, data=iris)
    ## model.actual <- ridgereg(Petal.Width ~ Petal.Length, data=iris)
    ## expect_equal(model.actual$call, "ridgereg(formula = Petal.Width ~ Petal.Length, data = iris)")
    ## check_model_methods(model.actual, model.expected)
    
    ## Advanced Model
    model.expected <- lm.ridge(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    model.actual <- ridgereg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    expect_equal(model.actual$call,
                 "ridgereg(formula = Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)")
    check_model_methods(model.actual, model.expected, iris[, c("Petal.Length", "Sepal.Width", "Sepal.Length")])

    ## Qualitative Model
    ## model.expected <- lm.ridge(Petal.Width ~ Species, data=iris)
    ## model.actual <- ridgereg(Petal.Width ~ Species, data=iris)
    ## expect_equal(model.actual$call, "ridgereg(formula = Petal.Width ~ Species, data = iris)")
    ## check_model_methods(model.actual, model.expected)
})

## test_that("ridgereg coefficient statistics are correct", {
##     check_coefficient_statistics <- function(actual, expected) {
##         coefficients <- summary(expected)$coef

##         expect_equal(actual$coefficients$val, coefficients[, 1])
##         expect_equal(actual$coefficients$se, coefficients[, 2])
##         expect_equal(actual$coefficients$tval, coefficients[, 3])
##         expect_equal(actual$coefficients$pval, coefficients[, 4])
##     }

##     ## Simple Model
##     ## model.expected <- lm.ridge(Petal.Width ~ Petal.Length, data=iris)
##     ## model.actual <- ridgereg(Petal.Width ~ Petal.Length, data=iris)
##     ## check_coefficient_statistics(model.actual, model.expected)    
    
##     ## Advanced Model
##     model.expected <- lm.ridge(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
##     model.actual <- ridgereg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
##     check_coefficient_statistics(model.actual, model.expected)

##     ## Qualitative Model
##     ## model.expected <- lm.ridge(Petal.Width ~ Species, data=iris)
##     ## model.actual <- ridgereg(Petal.Width ~ Species, data=iris)
##     ## check_coefficient_statistics(model.actual, model.expected)
## })
