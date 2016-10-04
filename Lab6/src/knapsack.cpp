#include <Rcpp.h>
#include <math.h>
#include <vector>
#include <limits>

// Brute Force
Rcpp::List knapsack_brute_force_cpp(const Rcpp::DataFrame& x, unsigned W);
std::vector<bool> create_bitstring(const std::vector<bool>& bistring);
Rcpp::IntegerVector get_elements(const std::vector<bool>& bitstring);

// Dynamic
Rcpp::List knapsack_dynamic_cpp(const Rcpp::DataFrame& x, unsigned W);

// Greedy
Rcpp::List knapsack_greedy_cpp(const Rcpp::DataFrame& x, unsigned W);

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
        current_weight += weights[k];
        current_value += values[k];
      }
    }

    if (current_value > best_value && current_weight < W) {
      best_value = current_value;
      best_weight = current_weight;
      best_choice = bitstring;
    }
  }

  Rcpp::IntegerVector elements = get_elements(best_choice);

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

Rcpp::IntegerVector get_elements(const std::vector<bool>& bitstring) {
  Rcpp::IntegerVector elements;

  for (unsigned i = 0; i < bitstring.size(); ++i) {
    if (bitstring.at(i)) {
      elements.push_back(i + 1);
    }
  }

  return elements;
}

// Dynamic Implementation -------------------------------------------------------
Rcpp::List knapsack_dynamic_cpp(const Rcpp::DataFrame& x, unsigned W) {
}

// Greedy Implementation --------------------------------------------------------
Rcpp::List knapsack_greedy_cpp(const Rcpp::DataFrame& x, unsigned W) {
}

