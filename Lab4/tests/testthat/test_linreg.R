library(Lab4)
context("linreg")

test_that("linreg of invalid input is invalid", {
    expect_error(linreg(1, iris))
    expect_error(linreg(y ~ x, 1))
    # The variables x, y does not exist in iris.
    expect_error(linreg(y ~ x, iris))
})

test_that("linreg of valid input is correct", {
    check_model_methods <- function(actual, expected) {
        expect_equal(actual$coef(), expected$coefficients)
        expect_equal(actual$resid(),expected$residuals)
        expect_equal(actual$pred(), predict(model.expected))
        expect_equal(actual$df, expected$df.residual)
    }

    ## Simple Model
    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)
    expect_equal(model.actual$call, "linreg(formula = Petal.Width ~ Petal.Length, data = iris)")
    check_model_methods(model.actual, model.expected)

    ## Advanced Model
    model.expected <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    expect_equal(model.actual$call,
                 "linreg(formula = Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)")
    check_model_methods(model.actual, model.expected)

    ## Qualitative Model
    model.expected <- lm(Petal.Width ~ Species, data=iris)
    model.actual <- linreg(Petal.Width ~ Species, data=iris)
    expect_equal(model.actual$call, "linreg(formula = Petal.Width ~ Species, data = iris)")
    check_model_methods(model.actual, model.expected)
})

test_that("linreg coefficient statistics are correct", {
    check_coefficient_statistics <- function(actual, expected) {
        coefficients <- summary(expected)$coefficients

        expect_equal(actual$coefficients$val, coefficients[, 1])
        expect_equal(actual$coefficients$se, coefficients[, 2])
        expect_equal(actual$coefficients$tval, coefficients[, 3])
        expect_equal(actual$coefficients$pval, coefficients[, 4])
    }

    ## Simple Model
    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)
    check_coefficient_statistics(model.actual, model.expected)

    ## Advanced Model
    model.expected <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    check_coefficient_statistics(model.actual, model.expected)

    ## Qualitative Model
    model.expected <- lm(Petal.Width ~ Species, data=iris)
    model.actual <- linreg(Petal.Width ~ Species, data=iris)
    check_coefficient_statistics(model.actual, model.expected)
})
