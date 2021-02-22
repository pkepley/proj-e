'''
    Problem 16:
    What is the sum of the digits of the number 2^1000?

    Solved: 2015-02-05
'''


def solve_prob():
    n = 2**1000
    digits = [int(d) for d in str(n)]

    return sum(digits)


if __name__ == '__main__':
    soln = solve_prob()
    print("Solution to Prob 16: {}".format(soln))
