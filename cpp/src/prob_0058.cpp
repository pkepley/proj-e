#include "prime_tools.hpp"
#include <cmath>
#include <iostream>
#include <vector>

#ifndef VERBOSE
#define VERBOSE 0
#endif

int64_t spiral_until_frac_primes_below_thresh(double thresh)
{
  int layers = 1;
  int n_on_diag = 1;
  int n_primes_on_diag = 0;
  int side_length = 1;
  double frac_primes = 0;
  PrimeTools pt = PrimeTools();

  do {
    layers++;
    n_on_diag += 4;
    side_length = 2 * (layers - 1) + 1;

    for (int i = 0; i < 4; i++) {
      int num_curr = side_length * side_length - i * (side_length - 1);

      if (pt.is_prime(num_curr)) {
        n_primes_on_diag++;
      }
    }

    frac_primes = ((double)n_primes_on_diag / (double)n_on_diag);

    #if VERBOSE
    std::cout << "------------------------------------------\n";
    std::cout << "side_length: " << side_length << "\n";
    std::cout << "n_primes: "  << n_primes_on_diag << "\n";
    std::cout << "n_on_diag: " << n_on_diag << "\n";
    std::cout << "frac primes: " << frac_primes << "\n";
    std::cout << "------------------------------------------\n\n";
    #endif
  }
  while (frac_primes > thresh);

  return side_length;
}


int64_t solve_prob_0058() {
  return spiral_until_frac_primes_below_thresh(0.10);
}


int main() {
  std::cout << "Solution to Problem 58: " << solve_prob_0058() << "\n";
  return 0;
}
