'''
    Prob 13:

    Work out the first ten digits of the sum of the following
    one-hundred 50-digit numbers.

    Solved: 2015-02-05
'''


def solve_prob():
    with open('../data/prob_0013.txt', 'r') as f:
        number_list = list(f)
        number_list = [int(n.rstrip()) for n in number_list]

    sum_of_numbers = sum(number_list)
    sum_of_numbers_str = str(sum_of_numbers)

    return sum_of_numbers_str[0:10]


if __name__ == "__main__":
    rslt = solve_prob()
    print("Solution to Problem 13: {}".format(rslt))
