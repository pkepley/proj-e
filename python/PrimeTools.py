from math import sqrt
from operator import mul
from bisect import bisect_right
from functools import reduce


class PrimeTools:
    '''
    ---------------------------------------------------------------------------
    PrimeTools
    ---------------------------------------------------------------------------
    PrimeTools(n) - use sieve to identify Primes up to and including n
                    returns a PrimeTools object
    extend(n)     - extend known primes up to and including n using the
                    previously identified primes (no return)
    is_prime(m)   - returns a Boolean, true if prime, false if not
    prime_divisors(m) - returns a list of integers containing the prime
                    divisors of m
    n_divisors(m) - returns an integer, the number of divisors of m
    n_primes_below(m) - returns an integer, the number of primes below m,
                        including m itself
    '''

    def __init__(self, n):
        self.n = n
        self.prime_list = []
        self.__sieve__()

    def extend(self, n):
        self.n = max(n, self.n)
        self.__sieve__()

    def __sieve__(self):
        # Generate the Sieve for marking
        sieve_list = [True for i in range(self.n+1)]
        sieve_list[0] = False
        sieve_list[1] = False

        if len(self.prime_list) == 0:
            p = 1
        else:
            # Already knew primes so mark these primes
            for p in self.prime_list:
                for j in range(2*p, self.n + 1, p):
                    sieve_list[j] = False

        while p < self.n:
            for i in range(p + 1, self.n + 1):
                if sieve_list[i]:
                    p = i
                    for j in range(2*p, self.n + 1, p):
                        sieve_list[j] = False
                    self.prime_list.append(p)
            else:
                p = self.n

    def __must_extend__(self, m):
        return len(self.prime_list) == 0 or self.n < abs(m)

    def is_prime(self, m):
        m = abs(m)
        if m == 0 or m == 1:
            return False
        else:
            sqrt_m = int(sqrt(m))
            if self.n < sqrt_m:
                self.extend(sqrt_m + 1)
            for p in self.prime_list:
                if m == p:
                    return True
                elif m % p == 0:
                    return False
            return True

    def prime_divisors(self, m):
        m = abs(m)
        prime_divisor_list = []

        # no primes divide 1
        if m == 1:
            return 1

        # needed in case n happens to be prime itself
        if self.__must_extend__(m):
            self.extend(m+1)

        k = m
        for p in self.prime_list:
            p_exp = 0
            if k == 1:
                break
            while not k % p:
                k /= p
                p_exp += 1
            if p_exp:
                prime_divisor_list.append((p, p_exp))

        return prime_divisor_list

    def n_divisors(self, m):
        m = abs(m)
        prime_divisors = self.prime_divisors(m)

        return reduce(mul, [p_exp+1 for (p, p_exp) in prime_divisors], 1)

    def n_primes_below(self, m):
        m = abs(m)

        if self.__must_extend__(m):
            self.extend(m+1)

        if self.prime_list[-1] < m:
            return len(self.prime_list)
        else:
            return bisect_right(self.prime_list, m)


if __name__ == "__main__":
    pt = PrimeTools(6)

    def passes_test(exp, obs):
        passes_test = exp == obs
        print("Passes (exp=obs?) {} ?= {} : {}".format(exp, obs, passes_test))
        return passes_test

    # Test 1
    n_under_obs = pt.n_primes_below(11232134)
    n_under_exp = 740826
    assert(n_under_obs == n_under_exp)
    passes_test(n_under_exp, n_under_obs)

    # Test 2
    biggest_p_exp = 11232083
    biggest_p_obs = pt.prime_list[-1]
    assert(biggest_p_exp == biggest_p_obs)
    passes_test(biggest_p_exp, biggest_p_obs)

    # Test 3
    # compute n_divisors of 11232134 = 2*127*44221
    # should be 2^3 = 8
    n_div_m_exp = 8
    n_div_m_obs = pt.n_divisors(11232134)
    assert(n_div_m_exp == n_div_m_obs)
    passes_test(n_div_m_exp, n_div_m_obs)
    
