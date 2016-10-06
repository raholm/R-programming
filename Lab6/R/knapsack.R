#' Knapsack Brute Force
#'
#' Solves 0-1 knapsack problem using brute force.
#'
#' @usage
#' knapsack_brute_force(x, W, fast=FALSE, parallel=FALSE)
#'
#' @param x A data frame containing the knapsack objects.
#' \describe{
#' \item{v}{The values of the objects.}
#' \item{w}{The weights of the objects.}
#' }
#' @param W The total amount of capacity available.
#' @param fast Set to FALSE by default. Will use a c++ implementation if set to TRUE.
#' @param parallel Set to FALSE by default. Will use a parallel implementation if set to TRUE and override fast=TRUE.
#' @return a List containing information about the solution.
#' \describe{
#' \item{value}{The best value found.}
#' \item{weight}{The weight of the best value.}
#' \item{elements}{The items picked to achieve the best solution.}
#' }
#'
#' @examples
#' knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500, fast=TRUE)
#' knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500, parallel=TRUE)
#'
#' @import parallel
#'
#' @export
knapsack_brute_force <- function(x, W, fast=FALSE, parallel=FALSE) {
    .check_input(x)

    result <- NULL

    if (parallel) {
        result <- knapsack_brute_force_parallel(x, W)
    } else if (fast) {
       result <- knapsack_brute_force_cpp(x, W)
    } else {
       result <- knapsack_brute_force_R(x, W)
    }

    return(result)
}

knapsack_brute_force_R <- function(x, W) {
    n <- nrow(x)

    best_value <- -Inf
    best_weight <- 0
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

knapsack_brute_force_parallel <- function(x, W) {
    n <- nrow(x)
    core_count <- detectCores()

    combinations <- 1:2^n
    combinations_per_core <- as.integer(length(combinations) / core_count + 0.5)

    cluster <- makeCluster(core_count)

    result <- parLapply(cluster, seq_len(core_count) - 1, function(i, x, W, combinations, combinations_per_core) {
        start_index <- (i * combinations_per_core + 1)
        end_index <- start_index + combinations_per_core
        core_combinations <- combinations[start_index:end_index]
        return(knapsack_brute_force_parallel_internal(x, W, core_combinations))
    }, x, W, combinations, combinations_per_core)

    best_result <- result[order(sapply(result,'[[', "value"), decreasing=TRUE)][[1]]

    stopCluster(cluster)
    return(best_result)
}

knapsack_brute_force_parallel_internal <- function(x, W, combinations) {
    n <- nrow(x)

    best_value <- -Inf
    best_weight <- 0
    best_choice <- NULL

    for (i in combinations) {
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
#' @usage
#' knapsack_dynamic(x, W, fast=FALSE)
#'
#' @param x A data frame containing the knapsack objects.
#' \describe{
#' \item{v}{The values of the objects.}
#' \item{w}{The weights of the objects.}
#' }
#'
#' @param W The total amount of capacity available.
#' @param fast Set to FALSE by default. Will use a c++ implementation if set to TRUE.
#' @return A list containing information about the solution.
#' \describe{
#' \item{value}{The best value found.}
#' \item{weight}{The weight of the best value.}
#' \item{elements}{The items picked to achieve the best solution.}
#' }
#'
#' @examples
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500, fast=TRUE)
#'
#' @export
knapsack_dynamic <- function(x, W, fast=FALSE) {
    .check_input(x)

    result <- NULL

    if (fast) {
        result <- knapsack_dynamic_cpp(x, W)
    } else {
        result <- knapsack_dynamic_R(x, W)
    }

    return(result)
}

knapsack_dynamic_R <- function(x, W) {
    n <- nrow(x)
    table <- matrix(data=0, nrow=n+1, ncol=W+1)

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
    best_choice <- knapsack_dynamic_R.best_choice(x, table)
    best_weight <- sum(x$w[best_choice])

    return(list(value=best_value, weight=best_weight, elements=best_choice))
}

knapsack_dynamic_R.best_choice <- function(x, table) {
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
#' Solves 0-1 knapsack problem using greedy heuristic. The heuristic used is value / weight.
#'
#' @usage
#' knapsack_greedy(x, W, fast=FALSE)
#'
#' @param x A data frame containing the knapsack objects.
#' \describe{
#' \item{v}{The values of the objects.}
#' \item{w}{The weights of the objects.}
#' }
#'
#' @param W The total amount of capacity available.
#' @param fast Set to FALSE by default. Will use a c++ implementation if set to TRUE.
#' @return A list containing information about the solution.
#' \describe{
#' \item{value}{The best value found.}
#' \item{weight}{The weight of the best value.}
#' \item{elements}{The items picked to achieve the best solution.}
#' }
#'
#' @examples
#' knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)
#' knapsack_greedy(x = knapsack_objects[1:800,], W = 3500, fast=TRUE)
#'
#' @export
knapsack_greedy <- function(x, W, fast=FALSE) {
    .check_input(x)

    result <- NULL

    if (fast) {
        result <- knapsack_greedy_cpp(x, W)
    } else {
        result <- knapsack_greedy_R(x, W)
    }

    return(result)
}

knapsack_greedy_R <- function(x, W) {
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

    return(list(value=as.integer(best_value + 0.5), weight=best_weight, elements=best_choice))
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
