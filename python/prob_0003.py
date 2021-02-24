'''
    Problem 3:
    Compute the largest prime factor of 600851475143

    Solved: 2014-08-21
'''


from PrimeTools import PrimeTools


def solve_prob(n=600851475143, pt=None):
    if pt is None:
        pt = PrimeTools(10**5)

    factorization = pt.factorize(n)
    prime_factors = [p for (p, e) in factorization]

    return max(prime_factors)


if __name__ == '__main__':
    soln = solve_prob()
    print("Solution to Prob 8: {}".format(soln))
