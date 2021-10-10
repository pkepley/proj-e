/*
    Problem 78:
    Find the smallest n for which 10^6 | P(n),
    where P(n) is the # of partitions of n.

    Solved: 2021-10-10
*/
#include <iostream>
#include <vector>

std::vector<int32_t> partitions(const unsigned int max_n, const unsigned int modulus) {
  std::vector < std::vector<uint32_t> > p(max_n + 1, std::vector<uint32_t> (max_n + 1));
  std::vector<int32_t> partitions(max_n + 1);

  // compute number of partitions of n of size k  (modulo the modulus)
  // https://www.whitman.edu/mathematics/cgt_online/book/section03.03.html
  for (int n = 1; n <= max_n; n++){
    for (int k = 1; k <= n; k++){
      if (k == 1)
        p[n][k] = 1;
      else
        p[n][k] = p[n - k][k] + p[n - 1][k-1];

      // modulo the modulus
      p[n][k] = p[n][k] % modulus;
    }
  }

  // compute number of partitions of n (modulo the modulus)
  for (int n = 1; n <= max_n; n++){
    for (int k = 1; k <= max_n; k++){
      partitions[n] += p[n][k];
      partitions[n] = partitions[n] % modulus;
    }
  }

  return partitions;
}


int32_t solve_prob_0078() {
  unsigned int n = 100000;
  unsigned int modulus = 1000000;
  std::vector<int32_t> v = partitions(n, modulus);

  // find first
  for (unsigned int i = 1; i <= n; i++) {
    if (v[i] == 0)
      return i;
  }

  // didn't find any
  return -1;
}


int main() {
  std::cout << "Solution to Problem 78: " << solve_prob_0078() << "\n";

  return 0;
}
