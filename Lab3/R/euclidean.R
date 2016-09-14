#' Finds the greatest common divisor of two numbers using the Euclidean algorithm.
#' 
#' @param a A number.
#' @param b A number.
#' @return The greatest common divisor of \code{a} and \code{b}.
#' 
#' @examples 
#' euclidean(100, 1000)
#' euclidean(1000, 100)
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
  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)
}
