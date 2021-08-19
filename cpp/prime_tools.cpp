#include <iostream>
#include <vector>
#include <tuple>
#include <math.h>


std::vector<int64_t> prime_sieve(int64_t n) {
  std::vector<int64_t> primes;
  std::vector<short> sieve(n + 1, 0);
  int64_t k;
  int64_t p = 2;
  int64_t n_primes = 0;

  // mark composite numbers with 1
  sieve[0] = 1;
  sieve[1] = 1;

  for (p = 2; p <= n;) {
    // current number is prime
    primes.push_back(p);
    n_primes += 1;
    sieve[p] = 0;

    // mark all multiples of p
    for (k = 2; k <= n / p; k++) {
      sieve[k * p] = 1;
    }

    // find next prime by finding the next non-composite
    // number
    k = p + 1;
    while (sieve[k] == 1)
      k++;
    p = k;
  }

  return primes;
}


class PrimeFactor {
public:
  int64_t divisor;
  int64_t exponent;

  PrimeFactor(int64_t d, int64_t e) {
    divisor = d;
    exponent = e;
  }
};


std::vector<PrimeFactor> factorize(int64_t n, std::vector<int64_t> primes)
{
  std::vector<PrimeFactor> factorization;
  int64_t m = n;

  for (int64_t i = 0; i < (int)(sqrt(n) + 1) & m > 1; i++)
    {
      int64_t exponent = 0;
      int64_t divisor = primes[i];

      while (m % divisor == 0) {
        m /= divisor;
        exponent++;
      }

      if (exponent > 0){
        factorization.push_back(PrimeFactor(divisor, exponent));
      }
    }

  // handle the case when n is prime
  if (m > 1)
    {
      factorization.push_back(PrimeFactor(m, 1));
    }

  return factorization;
}
