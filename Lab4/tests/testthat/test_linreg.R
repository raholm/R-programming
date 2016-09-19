library(Lab4)
context("linreg")

test_that("linreg of invalid input is invalid", {
    data(iris)

    expect_error(linreg(1, iris))
    expect_error(linreg(y ~ x, 1))
    expect_error(linreg(y ~ x, iris))
})

test_that("linreg of valid input is correct", {
    data(iris)

    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)

    expect_equal(deparse(model.actual$call), "linreg(formula = Petal.Width ~ Petal.Length, data = iris)")
    expect_equal(model.actual$coef(), model.expected$coefficients)
    expect_equal(model.actual$resid(), model.expected$residuals)
    expect_equal(model.actual$pred(), predict(model.expected))
    expect_equal(model.actual$df, model.expected$df.residuals)
})

test_that("linreg coefficient statistics are correct", {
    data(iris)

    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    coefficients <- summary(model.expected)$coefficients
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)

    expect_equal(model.actual$coefficients, coefficients[, 1])
    expect_equal(model.actual$coefficients.se, coefficients[, 2])
    expect_equal(model.actual$coefficients.tval, coefficients[, 3])
    expect_equal(model.actual$coefficients.pval, coefficients[, 4])
})
