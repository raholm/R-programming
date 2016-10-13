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
    expect_error(ridgereg(Petal.Width ~ Petal.Length + Sepal.Length, data=iris, lambda=-1))
})

fitted_values_lmridge <- function(model, data, intercept) {
    ## scaled_data <- scale(data, center = model$xm, scale = model$scales)
    data <- cbind(rep(1, nrow(data)), data)
    fitted_values <- data %*% c(intercept, model$coef)
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
}

check_model_methods <- function(actual, expected, data) {
    
    expect_equal(actual$coef()[-1], expected$coef)

    actual.prediction <- model.actual$pred()
    scaled_data <- scale(data, center=actual$center, scale=actual$scale)
    expect_equal(actual.prediction, fitted_values_lmridge(expected, scaled_data, intercept=actual$coef()[1]))
}

test_that("ridgereg of valid input is correct", {
    ## Advanced Model
    model.expected <- lm.ridge(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    model.actual <- ridgereg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)

    expect_equal(model.actual$call,
                 "ridgereg(formula = Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)")
    check_model_methods(model.actual, model.expected, iris[, c("Petal.Length", "Sepal.Width", "Sepal.Length")])

    ## Qualitative Model
    formula <- as.formula(Petal.Width ~ Species + Petal.Length)
    data <- model.matrix(formula, iris)[, -1]

    model.expected <- lm.ridge(formula, data=iris)
    model.actual <- ridgereg(Petal.Width ~ Species + Petal.Length, data=iris)
    expect_equal(model.actual$call, "ridgereg(formula = Petal.Width ~ Species + Petal.Length, data = iris)")
    check_model_methods(model.actual, model.expected, data)
})

test_that("new predictions are correct.", {
    formula <- as.formula(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length)

    model.expected <- lm.ridge(formula, iris)
    model.actual <- ridgereg(formula, iris)

    test_data <- as.matrix(data.frame(Petal.Length=c(2, 5, 8, -14, -3),
                                      Sepal.Width=c(12, 23, 5, -12, -7),
                                      Sepal.Length=c(12, 3, 2, 8, -22)))

    actual.prediction <- model.actual$pred(test_data)
    scaled_test_data <- scale(test_data, center=actual$center, scale=actual$scale)
    expect_equal(actual.prediction, fitted_values_lmridge(model.expected, scaled_test_data,
                                                                     intercept=model.actual$coef()[1]))
})

test_that("lambda works properly.", {
    ## Advanced Model
    formula <- as.formula(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length)

    model.expected <- lm.ridge(formula, iris, lambda=1)
    model.actual <- ridgereg(formula, iris, lambda=1)

    check_model_methods(model.actual, model.expected, iris[, c("Petal.Length", "Sepal.Width", "Sepal.Length")])

    ## Qualitative Model
    formula <- as.formula(Petal.Width ~ Species + Petal.Length)
    data <- model.matrix(formula, iris)[, -1]

    model.expected <- lm.ridge(formula, iris, lambda=1)
    model.actual <- ridgereg(formula, iris, lambda=1)

    check_model_methods(model.actual, model.expected, data)
})
