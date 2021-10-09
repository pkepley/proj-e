#include "prime_tools.hpp"
#include <cmath>
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

// check if two integers digits are permutations of one another
bool are_digit_perms(int64_t n, int64_t phi) {
  std::string n_digits_ord = std::to_string(n);
  std::string phi_digits_ord = std::to_string(phi);

  // sort digits
  std::sort(n_digits_ord.begin(),n_digits_ord.end());
  std::sort(phi_digits_ord.begin(),phi_digits_ord.end());

  return n_digits_ord == phi_digits_ord;
}

// find 1 < n < 1e7 minimizing n / phi(n)
int64_t solve_prob_0070() {
  PrimeTools pt = PrimeTools(1e6);
  float n_phi_ratio;
  float min_n_phi_ratio = 1e7 ;
  int64_t argmin_ratio = 1e7;

  for (int64_t n = 2; n < 1e7; n++) {
    int64_t phi = pt.totient(n);

    if (are_digit_perms(n, phi)) {
      n_phi_ratio = (float) n / (float) phi;

      if (n_phi_ratio < min_n_phi_ratio) {
        min_n_phi_ratio = n_phi_ratio ;
        argmin_ratio = n;
      }
    }
  }
  return argmin_ratio;
}


int main() {
  int64_t rslt;

  std::cout << "Solving Problem 70 by brute force. this might take a while! \n";
  rslt = solve_prob_0070();
  std::cout << "Solution to Problem 70: " << rslt << "\n";

  return 0;
}
