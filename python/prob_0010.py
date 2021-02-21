'''
    Problem 10:
    Compute the sum of all primes below 2 million

    Solved: 2014-08-21
'''


from PrimeTools import PrimeTools


def sum_primes_below(n, pt=None):
    '''
    ---------------------------------------------------------------------------
    sum_primes_below
    ---------------------------------------------------------------------------
    Sum primes below n, excluding n itself. I.e. it returns:
        sum([p : p prime, p < n])
    You can optionally provide a pre-computed PrimeTools object (pt) whose
    sieve will be extended if necessary.
    '''
    if pt is None:
        pt = PrimeTools(n)
    else:
        pt.extend(n)

    return sum([p for p in pt.prime_list if p < n])


def solve_prob(n, pt=None):
    return sum_primes_below(n, pt)


if __name__ == '__main__':
    rslt = solve_prob(2 * 10**6)
    print("Solution to Problem 10: {0}".format(rslt))
