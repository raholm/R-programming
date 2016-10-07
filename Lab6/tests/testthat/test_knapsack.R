library(Lab6)
context("Knapsack")

tests.brute_force <- function(...) {
    test_knapsack_objects <- knapsack_objects(2000)

    actual <- knapsack_brute_force(x = test_knapsack_objects[1:8,], W = 3500, ...)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_brute_force(x = test_knapsack_objects[1:12,], W = 3500, ...)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_brute_force(x = test_knapsack_objects[1:8,], W = 2000, ...)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))

    actual <- knapsack_brute_force(x = test_knapsack_objects[1:12,], W = 2000, ...)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))
}


test_that("Brute force solution is correct", {
    tests.brute_force()
})

test_that("Parallel brute force solution is correct", {
    tests.brute_force(parallel=TRUE)
})

test_that("Brute force solution using cpp is correct", {
    tests.brute_force(fast=TRUE)
})

tests.dynamic <- function(...) {
    test_knapsack_objects <- knapsack_objects(2000)

    actual <- knapsack_dynamic(x = test_knapsack_objects[1:8,], W = 3500, ...)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_dynamic(x = test_knapsack_objects[1:12,], W = 3500, ...)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_dynamic(x = test_knapsack_objects[1:8,], W = 2000, ...)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))

    actual <- knapsack_dynamic(x = test_knapsack_objects[1:12,], W = 2000, ...)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))
}

test_that("Dynamic solution is correct", {
    tests.dynamic()
})

test_that("Dynamic solution using cpp is correct", {
    tests.dynamic(fast=TRUE)
})

tests.greedy <- function(...) {
    test_knapsack_objects <- knapsack_objects(2000)

    actual <- knapsack_greedy(x = test_knapsack_objects[1:800,], W = 3500, ...)
    expect_equal(actual$value, 192647)
    expect_equal(actual$elements, c(92, 574, 472, 80, 110, 537, 332, 117, 37, 776,
                                    577, 288, 234, 255, 500, 794, 55, 290, 436, 346,
                                    282, 764, 599, 303, 345, 300, 243, 43, 747, 35,
                                    77, 229, 719, 564))

    actual <- knapsack_greedy(x = test_knapsack_objects[1:1200,], W = 2000, ...)
    expect_equal(actual$value, 212337)
    expect_equal(actual$elements, c(92, 574, 472, 80, 110, 840, 537, 1000, 332, 117,
                                    37, 1197, 1152, 947, 904, 776, 577, 288, 1147,
                                    1131, 234, 255, 1006, 833, 1176, 1092, 873, 828,
                                    1059, 500, 1090, 794, 1033))
}

test_that("Greedy solution is correct", {
    tests.greedy()
})

test_that("Greedy solution using cpp is correct", {
    tests.greedy(fast=TRUE)
})
