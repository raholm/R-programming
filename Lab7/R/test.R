test <- function() {
    library(MASS)

    fit <- lm.ridge(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)

    x <- iris[, c("Petal.Length", "Sepal.Width", "Sepal.Length")]
    y <- iris[, "Petal.Width"]

    ## They are basically the same (dont understand why they differ)
    sapply(x, sd)
    fit$scales

    ## Mean values
    sapply(x, mean)
    fit$xm

    mean(y)
    fit$ym

    coef(fit)
    fit$coef / fit$scales
    fit$Inter
}


ridgereg_test <- function() {
    library(MASS)

    model.expected <- lm.ridge(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)
    model.actual <- ridgereg(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data=iris)

    model.actual$coef()
    coef(model.expected)

    mean(model.actual$center)
    model.actual$center
    model.actual$scale

    model.expected$scales

    formula <- as.formula(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length)

    model.expected <- lm.ridge(formula, iris, lambda=1)
    model.actual <- ridgereg(formula, iris, lambda=1)

    model.actual$coef()
    coef(model.expected)
}
