/*
    Problem 4:
    Find the largest palindromic number that is a product
    of two three digit numbers.

    Solved: 2021-10-10
*/
#include <vector>
#include <iostream>

bool is_palindrome(int n) {
  std::vector<int> reversed_digits;

  while(n) {
    reversed_digits.push_back(n % 10);
    n /= 10;
  }

  // compare i^th digits in forward and reversed order
  int n_digits = reversed_digits.size();
  for(int i = 0; i < n_digits; i++) {
    // if digits don't match, it's not a palindrome.
    if (reversed_digits[i] != reversed_digits[n_digits - i - 1])
      return false;
  }
  // all digits matched, so it's a palindrome.
  return true;
}


int solve_prob_0004() {
  std::vector<int> palin_products;

  // loop through all distinct pairs of 3 digit numbers
  // and find all 3 digit product pair palindromes
  for (int i = 100; i <= 999; i++) {
    for (int j = i; j <= 999; j++) {
      int prod = i * j;

      if (is_palindrome(prod))
        palin_products.push_back(prod);
    }
  }

  // find the biggest 3 digit product pair palindrome
  int biggest_palin_prod = 0;
  for (long unsigned int i = 0; i < palin_products.size(); i++) {
    if (palin_products[i] > biggest_palin_prod)
      biggest_palin_prod = palin_products[i];
  }

  return biggest_palin_prod;
}


int main() {
  std::cout << "Solution to Problem 4: " << solve_prob_0004() << "\n";

  return 0;
}
