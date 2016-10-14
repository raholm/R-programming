library(Lab7)
context("ridgereg")

## This is a bloody mess

test_that("ridgereg of invalid input is invalid", {
    expect_error(ridgereg(1, datasets::iris))
    expect_error(ridgereg(y ~ x, 1))
    # The variables x, y does not exist in datasets::iris.
    expect_error(ridgereg(y ~ x, datasets::iris))
    expect_error(ridgereg(formula=y ~ x))
    expect_error(ridgereg(data=datasets::iris))
    expect_error(ridgereg(Petal.Width ~ Petal.Length + Sepal.Length, data=datasets::iris, lambda=-1))
})

fitted_values_lmridge <- function(model, data, intercept, coef_scale) {
    data <- cbind(rep(1, nrow(data)), data)
    ## Scales the coefficients the same as in our model and uses the same intercept
    fitted_values <- data %*% c(intercept, model$coef / coef_scale)
    fitted_values <- as.vector(fitted_values)
    names(fitted_values) <- 1:length(fitted_values)
    return(fitted_values)
}

check_model_methods <- function(actual, expected, data) {
    ## Check that every coefficient except the intercept matches reasonably well
    expect_equal(actual$coef()[-1], expected$coef, tolerance=1e-2)

    actual.prediction <- actual$pred()
    scaled_data <- scale(data, center=actual$center, scale=actual$scale)
    expect_equal(actual.prediction, fitted_values_lmridge(expected, scaled_data,
                                                          intercept=actual$coef()[1],
                                                          coef_scale=actual$scale),
                 tolerance=1e-2)
}

test_that("ridgereg of valid input is correct", {
    ## Advanced Model
    model.expected <- MASS::lm.ridge(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=datasets::iris)
    model.actual <- ridgereg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=datasets::iris)

    expect_equal(model.actual$call,
                 "ridgereg(formula = Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = datasets::iris)")
    check_model_methods(model.actual, model.expected, datasets::iris[, c("Petal.Length", "Sepal.Width", "Sepal.Length")])

    ## Qualitative Model
    formula <- as.formula(Petal.Width ~ Species + Petal.Length)
    data <- model.matrix(formula, datasets::iris)[, -1]

    model.expected <- MASS::lm.ridge(formula, data=datasets::iris)
    model.actual <- ridgereg(Petal.Width ~ Species + Petal.Length, data=datasets::iris)
    expect_equal(model.actual$call, "ridgereg(formula = Petal.Width ~ Species + Petal.Length, data = datasets::iris)")
    check_model_methods(model.actual, model.expected, data)
})

test_that("new predictions are correct.", {
    formula <- as.formula(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length)

    model.expected <- MASS::lm.ridge(formula, datasets::iris)
    model.actual <- ridgereg(formula, datasets::iris)

    test_data <- as.matrix(data.frame(Petal.Length=c(2, 5, 8, -14, -3),
                                      Sepal.Width=c(12, 23, 5, -12, -7),
                                      Sepal.Length=c(12, 3, 2, 8, -22)))

    actual.prediction <- model.actual$pred(test_data)
    scaled_test_data <- scale(test_data, center=model.actual$center, scale=model.actual$scale)
    expect_equal(actual.prediction, fitted_values_lmridge(model.expected, scaled_test_data,
                                                          intercept=model.actual$coef()[1],
                                                          coef_scale=model.actual$scale),
                 tolerance=1e-2)
})

test_that("lambda works properly.", {
    ## Advanced Model
    formula <- as.formula(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length)

    model.expected <- MASS::lm.ridge(formula, datasets::iris, lambda=1)
    model.actual <- ridgereg(formula, datasets::iris, lambda=1)

    check_model_methods(model.actual, model.expected, datasets::iris[, c("Petal.Length", "Sepal.Width", "Sepal.Length")])

    ## Qualitative Model
    formula <- as.formula(Petal.Width ~ Species + Petal.Length)
    data <- model.matrix(formula, datasets::iris)[, -1]

    model.expected <- MASS::lm.ridge(formula, datasets::iris, lambda=1)
    model.actual <- ridgereg(formula, datasets::iris, lambda=1)

    check_model_methods(model.actual, model.expected, data)
})
