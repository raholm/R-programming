#' Finds the greatest common divisor of two integers by using the Euclidean algorithm.
#' 
#' @param a&b Both are positive integers which are supposed to find the GCD of them.
#' @return The greatest common divisor of \code{a} and \code{b}.
#' 
#' @examples 
#' euclidean(100, 1000)
#' euclidean(1000, 100)
#'
#' @export
#' @source \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
euclidean <- function(a, b) {
    euclidean_check_input(a, b)

    while (b != 0)
    {
        tmp <- b
        b <- a %% b
        a <- tmp
    }

    return(a)
}

euclidean_check_input <- function(a, b) {
  stopifnot(is.numeric(a), length(a) == 1, a %% 1 == 0, a > 0)
  stopifnot(is.numeric(b), length(b) == 1, b %% 1 == 0, b > 0)
}

