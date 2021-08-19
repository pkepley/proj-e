#include <vector>
#include <bits/stdint-intn.h>

class PrimeFactor {
public:
  int64_t divisor;
  int64_t exponent;

  PrimeFactor(int64_t d, int64_t e);
};

std::vector<int64_t> prime_sieve(int64_t n);

bool is_prime(int64_t n, std::vector<int64_t> &primes);

std::vector<PrimeFactor> factorize(int64_t n, std::vector<int64_t> primes);
