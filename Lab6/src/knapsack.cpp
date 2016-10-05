#include "knapsack.h"

#include <math.h>
#include <limits>
#include <algorithm>


// Brute Force Implementation ---------------------------------------------------
// [[Rcpp::export]]
Rcpp::List knapsack_brute_force_cpp(const Rcpp::DataFrame& x, unsigned W) {
  Rcpp::NumericVector values = x["v"];
  Rcpp::IntegerVector weights = x["w"];

  unsigned n = values.size();

  double best_value = -1 * std::numeric_limits<double>::infinity();
  unsigned best_weight = 0;
  std::vector<bool> best_choice;

  // Loop variables
  unsigned iterations = std::pow(2, n);
  unsigned current_weight;
  double current_value;
  std::vector<bool> bitstring(n, false);

  for (unsigned i = 0; i < iterations; ++i) {
    bitstring = create_bitstring(bitstring);

    current_value = 0.0;
    current_weight = 0;

    for (unsigned k = 0; k < n; ++k) {
      if (bitstring.at(k)) {
        current_value += values[k];
        current_weight += weights[k];
      }
    }

    if (current_value > best_value && current_weight < W) {
      best_value = current_value;
      best_weight = current_weight;
      best_choice = bitstring;
    }
  }

  Rcpp::IntegerVector elements = get_elements_bitstring(best_choice);

  return Rcpp::List::create(Rcpp::Named("value", int(best_value + 0.5)),
                            Rcpp::Named("weight", best_weight),
                            Rcpp::Named("elements", elements));
}

std::vector<bool> create_bitstring(const std::vector<bool>& bitstring) {
  std::vector<bool> output(bitstring);
  int j = output.size() - 1;

  while (output.at(j) != 0 && j > 0) {
    output.at(j) = 0;
    j -= 1;
  }

  output.at(j) = 1;
  return output;
}

Rcpp::IntegerVector get_elements_bitstring(const std::vector<bool>& bitstring) {
  Rcpp::IntegerVector elements;

  for (unsigned i = 0; i < bitstring.size(); ++i) {
    if (bitstring.at(i)) {
      elements.push_back(i + 1);
    }
  }

  return elements;
}

// Dynamic Implementation -------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List knapsack_dynamic_cpp(const Rcpp::DataFrame& x, unsigned W) {
  // TODO: Implement
  Rcpp::NumericVector values = x["v"];
  Rcpp::IntegerVector weights = x["w"];

  unsigned n = values.size();

  Rcpp::NumericMatrix table(n+1, W+1);

  for (unsigned item = 1; item < (n + 1); ++item) {
    for (unsigned capacity = 0; capacity < (W + 1); ++capacity) {
      if (weights[item - 1] > capacity) {
        table(item, capacity) = table(item - 1, capacity);
      } else {
        table(item, capacity) = std::max<double>(table(item - 1, capacity),
                                                 values[item - 1] + table(item - 1, capacity - weights[item - 1]));
      }
    }
  }


  double best_value = table(n, W);
  Rcpp::IntegerVector elements = get_elements_table(table, weights);
  unsigned best_weight = get_weights(elements, weights);

  return Rcpp::List::create(Rcpp::Named("value", int(best_value + 0.5)),
                            Rcpp::Named("weight", best_weight),
                            Rcpp::Named("elements", elements));
}

Rcpp::IntegerVector get_elements_table(const Rcpp::NumericMatrix& table, const Rcpp::IntegerVector& weights) {
  unsigned item = table.nrow() - 1;
  unsigned capacity = table.ncol() - 1;

  Rcpp::IntegerVector elements;

  while (capacity > 0 && item > 0) {
    if (table(item, capacity) != table(item - 1, capacity)) {
      elements.push_back(item);
      capacity -= weights[item - 1];
    }

    item -= 1;
  }

  return elements.sort();
}

unsigned get_weights(const Rcpp::IntegerVector& elements, const Rcpp::IntegerVector& weights) {
  unsigned weight = 0;

  for (unsigned i = 0; i < elements.size(); ++i) {
    weight += weights[elements[i] - 1];
  }

  return weight;
}


// Greedy Implementation --------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List knapsack_greedy_cpp(const Rcpp::DataFrame& x, unsigned W) {
  // TODO: Implement
  double best_value = 0.0;
  unsigned best_weight = 0;
  Rcpp::IntegerVector elements;

  return Rcpp::List::create(Rcpp::Named("value", int(best_value + 0.5)),
                            Rcpp::Named("weight", best_weight),
                            Rcpp::Named("elements", elements));
}
