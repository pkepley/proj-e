'''
   Problem 5:
   2520 is the smallest number that can be divided by each of the numbers
   from 1 to 10 without any remainder.

   What is the smallest positive number that is evenly divisible by all
   of the numbers from 1 to 20?

   Solved: 2014-11-20
'''


from functools import reduce


def gcd(n, m):
    if abs(m) < 0 or abs(n) < 0:
        return gcd(abs(n), abs(m))

    if n < m:
        return gcd(m, n)

    r = n % m

    if r == 0:
        return m
    else:
        return gcd(m, r)


def lcm(n, m):
    return (m * n) // gcd(n, m)


def lcm_up_to_n(n):
    return reduce(lcm, range(2, n), 1)


def solve_prob_5(n=20):
    return lcm_up_to_n(n)


if __name__ == '__main__':
    prob_5_rslt = solve_prob_5()
    print("Solution to Problem 5: {0}".format(prob_5_rslt))
