#include "prime_tools.hpp"
#include <iostream>
#include <cmath>

// gcd
int gcd(unsigned int d, unsigned int n) {
  unsigned int r;

  // swap if out of order
  if (d > n) {
    unsigned int tmp = d;
    d = n;
    n = tmp;
  }

  // euclidean algorithm
  while (n != 0) {
    r = d % n;
    d = n;
    n = r;
  }

  return d;
}

// count the number of distinct reduced fractions in the
// set {n / d : n < d, d <= d_max, 1/3 < n/d < 1/2}
int64_t count_reduced_fractions_in_range(int d_max) {
  int64_t n_distinct = 0;

  for (int d = 2; d <= d_max; d++) {

    // 1/3 < d/n < 1/2
    for (int n = (unsigned int)ceil(d/3.0); n <= (unsigned int)floor(d/2.0); n++) {
      if (gcd(d, n) == 1) {
        n_distinct++;
      }
    }
  }

  // 1/3 and 1/2 were included in the sum, remove them from the count!
  n_distinct -= 2;

  return n_distinct;
}

// count the number of distinct reduced fractions below 12000
int64_t solve_prob_0073() {
  return count_reduced_fractions_in_range(12000);
}


int main() {
  int64_t rslt;

  rslt = solve_prob_0073();
  std::cout << "Solution to Problem 73: " << rslt << "\n";

  return 0;
}
