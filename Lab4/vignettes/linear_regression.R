## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(Lab4)
library(datasets)

data <- datasets::iris

## ------------------------------------------------------------------------
model <- linreg(formula = Petal.Length ~ Species, data=data)

## ------------------------------------------------------------------------
# Shows the coefficients that were estimated
model$coef()
# Shows the residuals.
head(model$resid())
# Shows the fitted values.
head(model$pred())

## ------------------------------------------------------------------------
# Shows how the linreg function was called and the coefficients.
model$print()
# Shows a detailed summary of the residuals and coefficients with related statistics.
model$summary()

## ---- fig.show='hold', fig.width=6, fig.height=5, fig.align='center'-----
model$plot()

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

