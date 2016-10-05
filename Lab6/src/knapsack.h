#ifndef KNAPSACK_H
#define KNAPSACK_H

#include <Rcpp.h>
#include <vector>

// Brute Force
Rcpp::List knapsack_brute_force_cpp(const Rcpp::DataFrame& x, unsigned W);
std::vector<bool> create_bitstring(const std::vector<bool>& bistring);
Rcpp::IntegerVector get_elements_bitstring(const std::vector<bool>& bitstring);

// Dynamic
Rcpp::List knapsack_dynamic_cpp(const Rcpp::DataFrame& x, unsigned W);
Rcpp::IntegerVector get_elements_table(const Rcpp::NumericMatrix& table,
                                       const Rcpp::IntegerVector& weights);
unsigned get_weights(const Rcpp::IntegerVector& elements, const Rcpp::IntegerVector& weights);

// Greedy
struct Comparitor
{
  const std::vector<double>* values;

  bool operator()(unsigned a, unsigned b)
  {
    return values->at(a) > values->at(b);
  }
};

Rcpp::List knapsack_greedy_cpp(const Rcpp::DataFrame& x, unsigned W);
std::vector<unsigned> sort_indexes(const std::vector<double>& values);

#endif // KNAPSACK_H
