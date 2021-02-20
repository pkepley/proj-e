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
    extend(n)     - extend known primes up to and including n using the
                    previously identified primes
    '''

    def __init__(self, n):
        self.n = n
        self.primeList = []
        self.sieve()

    def extend(self, n):
        self.n = max(n, self.n)
        self.sieve()

    def sieve(self):
        # Generate the Sieve for marking
        sieveList = [True for i in range(self.n+1)]
        sieveList[0] = False
        sieveList[1] = False

        if len(self.primeList) == 0:
            p = 1
        else:
            # Already knew primes so mark these primes
            for p in self.primeList:
                for j in range(2*p, self.n + 1, p):
                    sieveList[j] = False

        while p < self.n:
            for i in range(p + 1, self.n + 1):
                if sieveList[i]:
                    p = i
                    for j in range(2*p, self.n + 1, p):
                        sieveList[j] = False
                    self.primeList.append(p)
            else:
                p = self.n

    def mustExtend(self, m):
        return len(self.primeList) == 0 or self.n < abs(m)

    def isPrime(self, m):
        m = abs(m)
        if m == 0 or m == 1:
            return False
        else:
            sqrtM = int(sqrt(m))
            if self.n < sqrtM:
                self.extend(sqrtM + 1)
            for p in self.primeList:
                if m == p:
                    return True
                elif m % p == 0:
                    return False
            return True

    def primeDivisors(self, m):
        m = abs(m)
        primeDivisorList = []

        # no primes divide 1
        if m == 1:
            return 1

        # needed in case n happens to be prime itself
        if self.mustExtend(m):
            self.extend(m+1)

        k = m
        for p in self.primeList:
            powerOfP = 0
            if k == 1:
                break
            while not k % p:
                k /= p
                powerOfP += 1
            if powerOfP:
                primeDivisorList.append((p, powerOfP))

        return(primeDivisorList)

    def numDivisors(self, m):
        m = abs(m)
        primeDivisors = self.primeDivisors(m)
        return reduce(mul, [pThTuple[1]+1 for pThTuple in primeDivisors], 1)

    def numPrimesBelow(self, m):
        m = abs(m)

        if self.mustExtend(m):
            self.extend(m+1)

        if self.primeList[-1] < m:
            return len(self.primeList)
        else:
            return bisect_right(self.primeList, m)


if __name__ == "__main__":
    pt = PrimeTools(6)
    print(pt.primeList)

    def passes_test(exp, obs):
        passes_test = exp == obs
        print("Passes (exp=obs?) {} ?= {} : {}".format(exp, obs, passes_test))
        return passes_test

    # Test 1
    n_under_obs = pt.numPrimesBelow(11232134)
    n_under_exp = 740826
    passes_test(n_under_exp, n_under_obs)

    # Test 2
    biggest_p_exp = 11232083
    biggest_p_obs = pt.primeList[-1]
    passes_test(biggest_p_exp, biggest_p_obs)
