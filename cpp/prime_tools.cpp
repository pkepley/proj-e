#include <vector>
#include <math.h>
#include <algorithm>
#include "prime_tools.hpp"


PrimeFactor::PrimeFactor(int64_t d, int64_t e)
{
  divisor = d;
  exponent = e;
}


void PrimeTools::sieve(int64_t n)
{
  std::vector<short> sieve(n + 1, 0);
  int64_t k;
  int64_t p;
  int64_t n_primes = 0;

  // mark composite numbers with 1
  sieve[0] = 1;
  sieve[1] = 1;

  // mark off multiples of any known primes
  if (primes.size() > 0) {
    for (int i = 0; i < primes.size(); i++) {
      p = primes[i];
      sieve[p] = 0;

      // mark all multiples of p
      for (k = 2; k <= n / p; k++) {
        sieve[k * p] = 1;
      }
    }
  }
  else {
    p = 2;
    primes.push_back(p);
  }


  while (p <= n) {
    // current number is prime
    sieve[p] = 0;

    // mark all multiples of p
    for (k = 2; k <= n / p; k++) {
      sieve[k * p] = 1;
    }

    // find next prime by finding the next non-composite
    // number
    k = p + 1;
    while (sieve[k] == 1) k++;
    p = k;

    // if we're still in the range, p must be prime!
    if (p < n) {
      primes.push_back(p);
      n_primes += 1;
    }
  }

  // update some basic stats after sieve
  PrimeTools::post_sieve_update(n);
}


void PrimeTools::post_sieve_update(int64_t sieved_to_n)
{
  primes_known_below = sieved_to_n;
  sqrt_primes_known_below = (int64_t)sqrt(primes_known_below);
}


bool PrimeTools::is_prime(int64_t n)
{
  bool is_prime;
  int i;

  // resize prime sieve n is too big.
  // resize factor is 10x to avoid frequent resieves
  if (sqrt_primes_known_below < (int64_t)sqrt(n)){
    sieve((int64_t)pow(10, ceil(log10(n))) + 1);
  }

  // n is big, so check primes up to sqrt(n) for divisors
  if (n > primes.back()) {
    for(i = 0; (i < primes.size()) && (primes[i] < (int64_t) sqrt(n) + 1); i++){
      if ((n % primes[i]) == 0){
        return false;
      }
    }
    return true;
  }

  // n is smaller than our biggest prime, so binary search to see
  // if n is on our list of primes
  else {
    return std::binary_search(primes.begin(), primes.end(), n);
  }
}


std::vector<PrimeFactor> PrimeTools::factorize(int64_t n) {
  std::vector<PrimeFactor> factorization;
  int64_t m = n;

  for (int64_t i = 0; (i < (int)(sqrt(n) + 1)) && (m > 1); i++) {
    int64_t exponent = 0;
    int64_t divisor = primes[i];

    while (m % divisor == 0) {
      m /= divisor;
      exponent++;
    }

    if (exponent > 0) {
      factorization.push_back(PrimeFactor(divisor, exponent));
    }
  }

  // handle the case when n is prime
  if (m > 1) {
    factorization.push_back(PrimeFactor(m, 1));
  }

  return factorization;
}
