#include "prime_tools.hpp"
#include <iostream>

// count the number of distinct reduced fractions in the
// set {n / d : n < d, d <= d_max}
int64_t distinct_reduced_fractions(int64_t d_max) {
  PrimeTools pt = PrimeTools(d_max);
  int64_t n_distinct = 0;

  for (int64_t d = 2; d <= d_max; d++) {
    // there are totient(d) integers n < d coprime to d
    // hence totient(d) distinct fractions with denominator d
    n_distinct += pt.totient(d);
  }

  return n_distinct;
}


// count the number of distinct reduced fractions below 10^6
int64_t solve_prob_0072() {
  return distinct_reduced_fractions(1000000);
}


int main() {
  int64_t rslt;

  // std::cout << distinct_reduced_fractions(8) << "\n";
  rslt = solve_prob_0072();
  std::cout << "Solution to Problem 72: " << rslt << "\n";

  return 0;
}
