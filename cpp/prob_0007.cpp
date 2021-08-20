// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143?
#include "prime_tools.hpp"
#include <iostream>
#include <ostream>
#include <vector>

PrimeTools get_n_primes(int64_t n)
{
  int64_t m = 32;
  PrimeTools pt = PrimeTools(m);

  while (pt.primes.size() < n) {
    m = 2 * m;
    pt.sieve(m);
  }

  return pt;
}

int64_t solve_prob_0007()
{
  PrimeTools pt = get_n_primes(10001);

  return pt.primes[10001 - 1];
}


int main() {
  std::cout << "Solution to Problem 7: " << solve_prob_0007() << "\n";
  return 0;
}


