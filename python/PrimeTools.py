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
    is_prime(m)   - returns a boolean, true if prime, false if not 
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
        sieveList = [True for i in range(self.n+1)]
        sieveList[0] = False
        sieveList[1] = False

        if len(self.prime_list) == 0:
            p = 1
        else:
            # Already knew primes so mark these primes
            for p in self.prime_list:
                for j in range(2*p, self.n + 1, p):
                    sieveList[j] = False

        while p < self.n:
            for i in range(p + 1, self.n + 1):
                if sieveList[i]:
                    p = i
                    for j in range(2*p, self.n + 1, p):
                        sieveList[j] = False
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
            sqrtM = int(sqrt(m))
            if self.n < sqrtM:
                self.extend(sqrtM + 1)
            for p in self.prime_list:
                if m == p:
                    return True
                elif m % p == 0:
                    return False
            return True

    def prime_divisors(self, m):
        m = abs(m)
        primeDivisorList = []

        # no primes divide 1
        if m == 1:
            return 1

        # needed in case n happens to be prime itself
        if self.__must_extend__(m):
            self.extend(m+1)

        k = m
        for p in self.prime_list:
            powerOfP = 0
            if k == 1:
                break
            while not k % p:
                k /= p
                powerOfP += 1
            if powerOfP:
                primeDivisorList.append((p, powerOfP))

        return(primeDivisorList)

    def n_divisors(self, m):
        m = abs(m)
        prime_divisors = self.prime_divisors(m)

        return reduce(mul, [pThTuple[1]+1 for pThTuple in prime_divisors], 1)

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
