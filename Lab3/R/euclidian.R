euclidian <- function(a, b) {
    euclidian_check_input(a, b)

    while (b != 0)
    {
        tmp <- b
        b <- a %% b
        a <- tmp
    }

    return(a)
}

euclidian_check_input <- function(a, b) {
  stopifnot(is.numeric(a), length(a) == 1)
  stopifnot(is.numeric(b), length(b) == 1)
}
