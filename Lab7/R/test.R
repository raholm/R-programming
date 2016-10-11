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





