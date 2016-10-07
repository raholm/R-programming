#' Knapsack Objects
#'
#' Function to generate knapsack objects
#'
#' @param n The number of objects to create
#' @return A data frame containing n knapsack objects
#' @format A data frame with two variables.
#' \describe{
#' \item{w}{The weights of the objects.}
#' \item{v}{The values of the objects.}
#' }
#'
#' @importFrom stats runif
#'
#' @name knapsack_objects
#' @export
knapsack_objects <- function(n){
set.seed(42)
knapsack_df <- data.frame(w=sample(1:4000, size=n, replace=TRUE),
                               v=runif(n=n, 0, 10000))
return(knapsack_df)
}
