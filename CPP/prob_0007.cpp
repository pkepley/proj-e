// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143?
#include "prime_tools.hpp"
#include <iostream>
#include <ostream>
#include <vector>

std::vector<int64_t> get_n_primes(int64_t n)
{
  int64_t m = 4096;
  std::vector<int64_t> primes = prime_sieve(m);

  while (primes.size() < n)
    {
      m = 2 * m;
      primes = prime_sieve(m);
    }

  return primes;
}

int64_t solve_prob_0007()
{
  std::vector<int64_t> primes = get_n_primes(10001);

  return primes[10001 - 1];
}


int main() {
  std::cout << "Solution to Problem 7: " << solve_prob_0007() << "\n";
  return 0;
}


