#include "prime_tools.hpp"
#include <iostream>
#include <vector>
#include <cmath>


bool is_circular_prime(int64_t p, PrimeTools &pt)
{
  std::vector<int64_t> reversed_digits;
  int64_t m = p;
  int n_digits;

  // get digits
  reversed_digits.push_back(m % 10);
  while ((m = m / 10) != 0)
    reversed_digits.push_back(m % 10);

  // how many digits?
  n_digits = reversed_digits.size();

  // iterate through permutations of p 
  for(int j = 0; j < n_digits; j++) {
    // compute next permutation of n
    for (int i = n_digits - 1; i >= 0; i--) {
      if (i == n_digits - 1) m = 0;
      else m *= 10;
      m += reversed_digits[(i + j) % n_digits];
    }

    // early exit if current permutation is not prime
    if (!pt.is_prime(m)) return false;
  }

  // if we got here, all permutations were prime
  return true;
}


int64_t count_circular_primes(int64_t n)
{
  unsigned long long n_circular_primes = 0;
  PrimeTools pt = PrimeTools(n);

  for (int i = 0; i < pt.primes.size(); i++) {
    if (is_circular_prime(pt.primes[i], pt))
      n_circular_primes++;
  }

  return n_circular_primes;
}


int64_t solve_prob_0035() {
  return count_circular_primes(1000000);
}


int main() {
  std::cout << "Solution to Problem 35: " << solve_prob_0035() << "\n";

  return 0;
}
