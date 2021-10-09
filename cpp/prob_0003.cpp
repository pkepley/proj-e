// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143?
#include "prime_tools.hpp"
#include <iostream>
#include <cmath>
#include <vector>

int64_t solve_prob_0003()
{
  int64_t n = 600851475143;
  int64_t n_sqrt = (int64_t)sqrt(600851475143);
  PrimeTools pt = PrimeTools(100000);
  std::vector<PrimeFactor> prime_factors = pt.factorize(n);

  return prime_factors[prime_factors.size() - 1].divisor;
}


int main() {
  std::cout << "Solution to Problem 3: " << solve_prob_0003() << "\n";
  return 0;
}


