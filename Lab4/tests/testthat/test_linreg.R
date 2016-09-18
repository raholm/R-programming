library(Lab4)
context("linreg")

test_that("linreg of invalid input is invalid", {
    data(iris)

    expect_error(linreg(1, iris), "Formula has invalid input.")
    expect_error(linreg(y ~ x, 1), "Data has invalid input.")
    expect_error(linreg(y ~ x, iris), "Formula variables are not valid fields.")
})

test_that("linreg of valid input is correct", {
    data(iris)

    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)

    expect_equal(model.actual$coef(), model.expected$coefficients)
    expect_equal(model.actual$resid(), model.expected$residuals)
    expect_equal(model.actual$pred(), predict(model.expected))
})


## data(iris)
## model <- lm(Petal.Width ~ Petal.Length, data=iris)
## model
## model$coefficients
## model$residuals
## model$fitted.value
## predict(model)
## summary(model)
