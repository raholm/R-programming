#' Internal Functions
#'
#' All the internal functions used in this package
#' (DO NOT EXPORT)
.format_number <- function(number, decimals, ...) {
    formatted <- as.numeric(format(round(number, decimals), nsmall=decimals, ...))
    names(formatted) <- names(number)
    return(formatted)
}

.readkey <- function() {
    cat ("Press [enter] to continue")
    line <- readline()
}
