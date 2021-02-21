'''
    Problem 9:
    Find the unique Pythagorean triple satisfying a + b + c = 1000

    Solved: 2014-08-21
'''


from math import sqrt


def solve_prob():
    for a in range(1, 1000):
        # using a + b + c = 1000, and a^2 + b^2 = c^2,
        # it follows that b must satisfy:
        b = 1000 * (500 - a) / (1000 - a)

        # a is an integer, so if b is an integer, then 
        # c = 1000 - (a+b) ==> c is an integer too. so we will
        # have found the desired triple if b is an integer:
        if b.is_integer():
            b = int(b)
            break

    # compute c
    c = int(sqrt(a**2 + b**2))

    return a * b * c


if __name__ == '__main__':
    soln = solve_prob()
    print("Solution to Prob 9: {}".format(soln))
