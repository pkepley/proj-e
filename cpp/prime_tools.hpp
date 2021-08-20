#include <vector>
#include <bits/stdint-intn.h>


class PrimeFactor
{
public:
  int64_t divisor;
  int64_t exponent;

  PrimeFactor(int64_t d, int64_t e);
};


class PrimeTools
{
public:
  std::vector<int64_t> primes;

  PrimeTools() { PrimeTools::sieve(1000); }
  PrimeTools(int64_t n) { PrimeTools::sieve(n); }

  void sieve(int64_t n);
  bool is_prime(int64_t n);
  std::vector<PrimeFactor> factorize(int64_t n);

private:
  int64_t primes_known_below = 0;
  int64_t sqrt_primes_known_below = 0;
  void post_sieve_update(int64_t sieved_to_n);
};
