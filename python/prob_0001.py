''' Problem 1:
    calculate the sum of all integers in [1,1000)
    which satisfy 3|x or 5|x
'''


def dividesIt(x, d):
    return int(x/d) * d == x


def solve_prob_1(n=1000):
    return sum([x for x in range(1, n) if dividesIt(x, 3) or
                dividesIt(x, 5)])


if __name__ == "__main__":
    prob_1_rslt = solve_prob_1()
    print("Solution to Problem 1: {}".format(prob_1_rslt))
