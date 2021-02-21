'''
    Problem 12:
    Find the first triangular number with d(T_N) > 500

    Solved: 2014-08-21
'''


from PrimeTools import PrimeTools


def triangle(n):
    '''
    ---------------------------------------------------------------------------
    triangle
    ---------------------------------------------------------------------------
    return the nth triangular number n*(n+1)/2
    '''
    return n*(n+1)//2


def first_triangular_with_k_divisors(k, lo=2, hi=10**3, pt=None):
    '''
    ---------------------------------------------------------------------------
    first_triangular_with_k_divisors
    ---------------------------------------------------------------------------
    Returns the first triangular number that has k divisors.

    Searches in the range [lo, hi) initially, if it fails to find a result,
    then it doubles the search range by 2 (i.e. lo <- hi, hi <- 2*hi) and
    searches there. You can optionally provide lo and hi to start the search,
    otherwise, these default to 2 and 10**3.

    Uses a PrimeTools object to compute divisors, which can optionally be
    provided. If not provided, will use PrimeTools(hi * (hi+1) / 2).
    '''
    if pt is None:
        pt = PrimeTools(triangle(hi))
    else:
        pt.extend(triangle(hi))

    for i in range(lo, hi):
        tri = triangle(i)
        n_divisors = pt.n_divisors(tri)
        if n_divisors > k:
            return tri

    else:
        return first_triangular_with_k_divisors(k, lo=hi, hi=2*hi, pt=pt)


def solve_prob(k=500, pt=None):
    return first_triangular_with_k_divisors(k, lo=2, hi=10**3, pt=pt)


if __name__ == "__main__":
    rslt = solve_prob()
    print("Solution to Problem 12: {}".format(rslt))
