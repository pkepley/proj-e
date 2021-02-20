'''
   problem 7:
   What is the 10,0001st prime number?

   Solved: 2014-08-21
'''


from PrimeTools import PrimeTools


# find the nth prime
def find_nth_prime(n, pt=None):
    if pt is None:
        # if no primetools object has been provided, then
        # begin by finding all primes below 100
        m = 100
        pt = PrimeTools(m)

    # keep extending prime list until we have n primes
    while len(pt.primeList) < n:
        m = 2 * m
        pt.extend(m)

    return pt.primeList[n-1]


def solve_prob(n=10001):
    return find_nth_prime(n)


if __name__ == '__main__':
    soln = solve_prob()
    print("Solution to Prob 7: {}".format(soln))
