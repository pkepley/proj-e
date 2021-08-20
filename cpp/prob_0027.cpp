#include "prime_tools.hpp"
#include <iostream>
#include <vector>
#include <cmath>


int count_consecutive_prime_from_quadratic(int a, int b, PrimeTools &pt)
{
  int64_t n = 0;

  while (pt.is_prime(n*n + a*n + b))  n++;

  return n;
}


int64_t solve_prob_0027() {
  PrimeTools pt = PrimeTools(10000);
  int n_consecutive = 0;
  int best_a = 0, best_b = 0;
  int most_consecutive = 0;

  for (int a=-999; a < 1000; a++){
    for(int b=-1000; b <= 1000; b++){
      n_consecutive = count_consecutive_prime_from_quadratic(a, b, pt);

      if (n_consecutive > most_consecutive){
        most_consecutive = n_consecutive;
        best_a = a;
        best_b = b;
      }
    }
  }

  return best_a * best_b;
}


int main() {
  std::cout << "Solution to Problem 27: " << solve_prob_0027() << "\n";

  return 0;
}
