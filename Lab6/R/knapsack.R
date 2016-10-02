knapsack_brute_force <- function(x, W) {
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

knapsack_dynamic <- function(x, W) {
}

knapsack_greedy <- function(x, W) {
}
