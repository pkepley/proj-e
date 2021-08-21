#include "prime_tools.hpp"
#include <cmath>
#include <iostream>
#include <vector>


int64_t first_of_n_consecutive_with_n_factors(int n)
{
  int max_n  = 100000;
  PrimeTools pt = PrimeTools(max_n);
  int n_consecutive_found = 0;
  int i = 0;

  for (i = 0; n_consecutive_found < n; i++)
  {
    // get more primes if we're running out 
    if ((i > max_n)){
       max_n *= 2;
       pt.sieve(max_n);
    }

    if (pt.factorize(i).size() == n) n_consecutive_found++;
    else  n_consecutive_found = 0;
  }

  return i - n;
}


int64_t solve_prob_0047() {
  return first_of_n_consecutive_with_n_factors(4);
}


int main() {
  std::cout << "Solution to Problem 47: " << solve_prob_0047() << "\n";
  return 0;
}
