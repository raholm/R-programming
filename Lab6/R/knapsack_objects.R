#' Knapsack Objects
#'
#' A data set containing knapsack objects
#'
#' @format A data frame with two variables.
#' \describe{
#' \item{w}{The weights of the objects.}
#' \item{v}{The values of the objects.}
#' }
#'
#' @name knapsack_objects
#' @export
set.seed(42)
n <- 1000000
knapsack_objects <- data.frame(w=sample(1:4000, size=n, replace=TRUE),
                               v=runif(n=n, 0, 10000))

set.seed(42)
n <- 2000
test_knapsack_objects <- data.frame(w=sample(1:4000, size=n, replace=TRUE),
                               v=runif(n=n, 0, 10000))
