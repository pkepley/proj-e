#include "prime_tools.hpp"
#include <cmath>
#include <iostream>
#include <vector>


bool is_truncatable_prime(int64_t p, PrimeTools &pt)
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

  // iterate through left and right truncations of p 
  for(int j = 0; j < n_digits; j++) {
    // compute next truncation of p
    for (int i = n_digits - 1; i >= j; i--) {
      if (i == n_digits - 1) m = 0;
      else m *= 10;
      m += reversed_digits[i];
    }
    // early exit if truncation is not prime
    if (!pt.is_prime(m))
      return false;

    // compute next truncation from other side of p
    for (int i = 0; i <= j; i++) {
      if (i == 0)
        m = 0;
      else
        m *= 10;
      m += reversed_digits[j - i];
    }
    // early exit if truncation is not prime
    if (!pt.is_prime(m))
      return false;
  }

  // if we got here, all permutations were prime
  return true;
}


int64_t sum_truncatable_primes()
{
  int64_t max_n = 4096;
  PrimeTools pt = PrimeTools(max_n);
  int n_truncatable = 11;
  int n_found = 0;
  int i=0;
  unsigned long long truncatable_prime_sum = 0;

  //2,3,5,7 are not truncatable (0, 1, 2, 3)
  for (i = 4; n_found < n_truncatable; i++)
  {
    //std::cout << i << "\n";
    // if we've run out of primes to check, get more
    if (i == pt.primes.size()){
      max_n *= 2;
      pt.sieve(max_n);
    }

    // found truncatable? handle it!
    if (is_truncatable_prime(pt.primes[i], pt)) {
      truncatable_prime_sum += pt.primes[i];
      n_found++;
    }
  }

  return truncatable_prime_sum;
}


int64_t solve_prob_0037() {
  return sum_truncatable_primes();
}


int main() {
  std::cout << "Solution to Problem 37: " << solve_prob_0037() << "\n";

  return 0;
}
