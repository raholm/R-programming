#' title here
#'
#' description here
#'
#' @param param description
#' @return
#'
#' @examples
#' examples here
#'
#' @export
#' @source \url{}
linreg <- function(formula, data)
{
    linreg_check_input(formula, data)

    ## Independent variable data
    X <- model.matrix(object=formula, data=data)

    ## Dependent variable data
    y_variables <- all.vars(formula)[!(all.vars(formula) %in% colnames(X))]
    y <- data[, y_variables]
}

linreg_check_input <- function(formula, data)
{
    stopifnot(length(all.vars(formula)) > 0)
    stopifnot(is.data.frame(data))

    ## Ensures that all variables in the formula
    ## are present in the data frame
    variables <- all.vars(formula)
    for (variable in variables)
    {
        stopifnot(variable %in% names(data))
    }
}

