#ifndef KNAPSACK_H
#define KNAPSACK_H

#include <Rcpp.h>
#include <vector>

// Brute Force
Rcpp::List knapsack_brute_force_cpp(const Rcpp::DataFrame& x, unsigned W);
std::vector<bool> create_bitstring(const std::vector<bool>& bistring);
Rcpp::IntegerVector get_elements(const std::vector<bool>& bitstring);

// Dynamic
Rcpp::List knapsack_dynamic_cpp(const Rcpp::DataFrame& x, unsigned W);

// Greedy
Rcpp::List knapsack_greedy_cpp(const Rcpp::DataFrame& x, unsigned W);

#endif // KNAPSACK_H
