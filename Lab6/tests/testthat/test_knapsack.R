library(Lab6)
context("Knapsack")

test_that("Brute force solution is correct", {
    actual <- knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_brute_force(x = knapsack_objects[1:12,], W = 3500)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))

    actual <- knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))
})

test_that("Brute force solution using cpp is correct", {
    actual <- knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500, fast=TRUE)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_brute_force(x = knapsack_objects[1:12,], W = 3500, fast=TRUE)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000, fast=TRUE)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))

    actual <- knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000, fast=TRUE)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))
})


test_that("Dynamic solution is correct", {
    actual <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))

    actual <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))
})

test_that("Dynamic solution using cpp is correct", {
    actual <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500, fast=TRUE)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500, fast=TRUE)
    expect_equal(actual$value, 16770)
    expect_equal(actual$elements, c(5, 8))

    actual <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000, fast=TRUE)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))

    actual <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000, fast=TRUE)
    expect_equal(actual$value, 15428)
    expect_equal(actual$elements, c(3, 8))
})

test_that("Greedy solution is correct", {
    actual <- knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)
    expect_equal(actual$value, 192647 + 2277)
    expect_equal(actual$elements, c(92, 574, 472, 80, 110, 537, 332, 117, 37, 776,
                                    577, 288, 234, 255, 500, 794, 55, 290, 436, 346,
                                    282, 764, 599, 303, 345, 300, 243, 43, 747, 35,
                                    77, 229, 719, 564, 401))

    actual <- knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000)
    expect_equal(actual$value, 212337 + 961)
    expect_equal(actual$elements, c(92, 574, 472, 80, 110, 840, 537, 1000, 332, 117,
                                    37, 1197, 1152, 947, 904, 776, 577, 288, 1147,
                                    1131, 234, 255, 1006, 833, 1176, 1092, 873, 828,
                                    1059, 500, 1090, 794, 1033, 1134))
})

test_that("Greedy solution using cpp is correct", {
    actual <- knapsack_greedy(x = knapsack_objects[1:800,], W = 3500, fast=TRUE)
    expect_equal(actual$value, 192647 + 2277)
    expect_equal(actual$elements, c(92, 574, 472, 80, 110, 537, 332, 117, 37, 776,
                                    577, 288, 234, 255, 500, 794, 55, 290, 436, 346,
                                    282, 764, 599, 303, 345, 300, 243, 43, 747, 35,
                                    77, 229, 719, 564, 401))

    actual <- knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000, fast=TRUE)
    expect_equal(actual$value, 212337 + 961)
    expect_equal(actual$elements, c(92, 574, 472, 80, 110, 840, 537, 1000, 332, 117,
                                    37, 1197, 1152, 947, 904, 776, 577, 288, 1147,
                                    1131, 234, 255, 1006, 833, 1176, 1092, 873, 828,
                                    1059, 500, 1090, 794, 1033, 1134))
})
