'''
    Problem 6:

    Find the difference between the sum of the squares of the first
    one hundred natural numbers and the square of the sum.

    Solved: 2015-02-16
'''


def solve_prob_6(n=100):
    sum_of_squares = sum([x**2 for x in range(1, n+1)])
    square_of_sum = sum([x for x in range(1, n+1)])**2

    return square_of_sum - sum_of_squares


if __name__ == '__main__':
    prob_6_rslt = solve_prob_6()
    print("Solution to Problem 6: {0}".format(prob_6_rslt))
