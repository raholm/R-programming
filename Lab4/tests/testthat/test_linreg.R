library(Lab4)
context("linreg")

data(iris)

test_that("linreg of invalid input is invalid", {
    expect_error(linreg(1, iris))
    expect_error(linreg(y ~ x, 1))
    expect_error(linreg(y ~ x, iris))
})

test_that("linreg of valid input is correct (simple model)", {
    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)

    expect_equal(deparse(model.actual$call), "linreg(formula = Petal.Width ~ Petal.Length, data = iris)")
    expect_equal(model.actual$coef(), model.expected$coefficients)
    expect_equal(model.actual$resid(), model.expected$residuals)
    expect_equal(model.actual$pred(), predict(model.expected))
    expect_equal(model.actual$df, model.expected$df.residual)
})

test_that("linreg of valid input is correct (advanced model)", {
    model.expected <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    model.actual <- linreg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)

    ## The string has been divided into multiple string due to its length.
    ## This puts it back to a single string.
    call_string <- gsub(" +", " ", paste(deparse(model.actual$call), collapse=""))
    expect_equal(call_string,
                 "linreg(formula = Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)")
    expect_equal(model.actual$coef(), model.expected$coefficients)
    expect_equal(model.actual$resid(), model.expected$residuals)
    expect_equal(model.actual$pred(), predict(model.expected))
    expect_equal(model.actual$df, model.expected$df.residual)
})

test_that("linreg coefficient statistics are correct (simple model)", {
    model.expected <- lm(Petal.Width ~ Petal.Length, data=iris)
    coefficients <- summary(model.expected)$coefficients
    model.actual <- linreg(Petal.Width ~ Petal.Length, data=iris)

    expect_equal(model.actual$coefficients$val, coefficients[, 1])
    expect_equal(model.actual$coefficients$se, coefficients[, 2])
    expect_equal(model.actual$coefficients$tval, coefficients[, 3])
    expect_equal(model.actual$coefficients$pval, coefficients[, 4])
})

test_that("linreg coefficient statistics are correct (advanced model)", {
    model.expected <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    coefficients <- summary(model.expected)$coefficients
    model.actual <- linreg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)

    expect_equal(model.actual$coefficients$val, coefficients[, 1])
    expect_equal(model.actual$coefficients$se, coefficients[, 2])
    expect_equal(model.actual$coefficients$tval, coefficients[, 3])
    expect_equal(model.actual$coefficients$pval, coefficients[, 4])
})
