## DO NOT UNCOMMENT

## library(lineprof)
## library(Lab6)

## brute1 <- lineprof(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500))
## shine(brute1)
## brute2 <- lineprof(knapsack_brute_force(x = knapsack_objects[1:20,], W = 3500, fast=TRUE))
## shine(brute2)
## brute3 <- lineprof(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500, parallel=TRUE))
## shine(brute3)

## dynamic1 <- lineprof(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
## shine(dynamic1)
## dynamic2 <- lineprof(knapsack_dynamic(x = knapsack_objects[1:50,], W = 20000, fast=TRUE))
## shine(dynamic2)

## greedy1 <- lineprof(knapsack_greedy(x = knapsack_objects[1:800,], W = 3500))
## shine(greedy1)
## greedy2 <- lineprof(knapsack_greedy(x = knapsack_objects, W = 50000, fast=TRUE))
## shine(greedy2)
