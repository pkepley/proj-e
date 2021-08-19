#include "prime_tools.hpp"
#include <iostream>
#include <vector>

unsigned long long sum_primes(int64_t n)
{
  unsigned long long prime_sum = 0;
  std::vector<int64_t> primes = prime_sieve(n);

  for (int i = 0; i < primes.size(); i++)
    prime_sum += (unsigned long long) primes[i];

  return prime_sum;
}

int64_t solve_prob_0010() {
  return sum_primes(2000000);
}

int main() {
   //std::cout << "Solution to Problem 10: " << sum_primes(10) << "\n";
   std::cout << "Solution to Problem 10: " << solve_prob_0010() << "\n";
   return 0;
}
