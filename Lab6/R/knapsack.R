#' Knapsack Brute Force
#'
#' Solves 0-1 knapsack problem using brute force.
#'
#' @export
knapsack_brute_force <- function(x, W, fast=FALSE) {
    .check_input(x)

    if (fast) {
       result <- knapsack_brute_force_cpp(x, W)
    } else {
       result <- knapsack_brute_force_R(x, W)
    }

    return(result)
}

knapsack_brute_force_R <- function(x, W) {
    n <- nrow(x)

    best_value <- -Inf
    best_weight <- Inf
    best_choice <- NULL

    for (i in 1:2^n) {
        bitstring <- intToBits(i)

        current_weight <- 0
        current_value <- 0

        for (k in 1:n) {
            if (bitstring[k] == 1) {
                current_weight <- current_weight + x$w[k]
                current_value <- current_value + x$v[k]
            }
        }

        if (current_value > best_value && current_weight < W) {
            best_value <- current_value
            best_weight <- current_weight
            best_choice <- bitstring
        }
    }

    return(list(value=as.integer(best_value + 0.5), weight=best_weight, elements=which(best_choice == 1)))
}

#' Knapsack Dynamic
#'
#' Solves 0-1 knapsack problem using dynamic programming.
#'
#' @export
knapsack_dynamic <- function(x, W) {
    .check_input(x)

    n <- nrow(x)
    table <- matrix(data=0, nrow=n+1, ncol=W+1)
    keep <- matrix(data=0, nrow=n+1, ncol=W+1)

    for (item in 1:n) {
        for (capacity in 1:W) {
            capacity_index <- capacity + 1

            if (x$w[item] > capacity) {
                table[item + 1, capacity_index] <- table[item, capacity_index]
            } else {
                value_without_current_item <- table[item, capacity_index]
                value_with_current_item <- x$v[item] + table[item, capacity_index - x$w[item]]

                table[item + 1, capacity_index] <- max(value_without_current_item,
                                                       value_with_current_item)
            }
        }
    }

    best_value <- as.integer(table[nrow(table), ncol(table)] + 0.5)
    best_choice <- knapsack_dynamic.best_choice(x, table)
    best_weight <- sum(x$v[best_choice])

    return(list(value=best_value, weight=best_weight, elements=best_choice))
}

knapsack_dynamic.best_choice <- function(x, table) {
    item <- nrow(table)
    capacity <- ncol(table) - 1

    best_choice <- c()

    while (capacity > 0 && item > 1) {
        if (table[item, capacity] != table[item - 1, capacity]) {
            best_choice <- c(best_choice, item - 1)
            capacity <- capacity - x$w[item - 1]
        }

        item <- item - 1
    }

    return(sort(best_choice))
}

#' Knapsack Greedy
#'
#' Solves 0-1 knapsack problem using greedy heuristic.
#'
#' @export
knapsack_greedy <- function(x, W) {
    .check_input(x)

    n <- nrow(x)
    ordered_items <- order(x$v / x$w, decreasing=TRUE)

    best_value <- 0
    best_weight <- 0
    best_choice <- c()

    capacity <- W
    item <- 1

    while (capacity > 0 && item <= n) {
        ordered_item <- ordered_items[item]

        if (x$w[ordered_item] <= capacity) {
            best_value <- best_value + x$v[ordered_item]
            best_weight <- best_weight + x$w[ordered_item]
            best_choice <- c(best_choice, ordered_item)

            capacity <- capacity - x$w[ordered_item]
        }

        item <- item + 1
    }

    return(list(value=as.integer(best_value), weight=best_weight, elements=best_choice))
}

.check_input <- function(x) {
    valid <- FALSE

    if (is.data.frame(x) && x$v>0 && x$w>0){
        valid <- TRUE
    }

    if (!valid) {
        stop("Invalid input.")
    }

    return(invisible())
}
