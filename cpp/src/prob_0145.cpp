/*
    Problem 145:
    Find the largest palindromic number that is a product
    of two three digit numbers.

    Solved: 2021-10-11
*/
#include <vector>
#include <iostream>


int reverse(int n) {
  int n_reversed = 0;

  while(n) {
    n_reversed *= 10;
    n_reversed += (n % 10);
    n /= 10;
  }

  return n_reversed;
}


bool is_odd_digit(int d) {
  if (d > 10)
    return false;
  else
    return (d == 1) | (d == 3) | (d == 5) | (d == 7) | (d == 9);
}


bool is_reversible(int n) {
  int n_reversed = reverse(n);
  int sum = n + n_reversed;
  bool is_reversible = true;

  is_reversible = is_reversible & !(n % 10 == 0);
  is_reversible = is_reversible & !(n_reversed % 10 == 0);

  while ((sum > 0) & is_reversible) {
    int d = (sum % 10);
    is_reversible = is_reversible & is_odd_digit(d);
    sum /= 10;
  }

  return is_reversible;
}


int count_reversible_below(int n) {
  int n_reversible = 0;

  for (int i = 0; i < n; i++){
    if (is_reversible(i))
      n_reversible++;
  }

  return n_reversible;
}


int solve_prob_0145() {
  return count_reversible_below(1000000000);
}


int main() {
  std::cout << "Solution to Problem 145: " << solve_prob_0145() << "\n";

  return 0;
}
