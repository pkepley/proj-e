'''
   Problem 8:
   Given a 1000 digit string of integers, find the 13 consecutive
   integers with largest product.

   The 1000 digit string is stored in ../data/prob_0008.txt

   Solved: 2014-08-21
'''


from functools import reduce
from operator import mul


def read_input():
    '''
    read_input: parse the input file
    '''
    with open('../data/prob_0008.txt') as f:
        numberList = [int(c) for line in f for c in line.rstrip()]

    return numberList


def largest_consecutive_product(number_list, n_consecutive):
    '''
    ---------------------------------------------------------------------------
    largest_consecutive_product
    ---------------------------------------------------------------------------
    Given a list of numbers, number_list, determine the largest product
    of n_consecutive elements of the list.
    '''

    n_list = len(number_list)

    if n_consecutive > n_list:
        largest_prod = reduce(mul, number_list, 1)

    else:
        largest_prod = 0

        for i in range(0, n_list - (n_consecutive+1)):
            product = reduce(mul, number_list[i:i+n_consecutive], 1)

            if product > largest_prod:
                largest_prod = product

    return largest_prod


def solve_prob():
    '''
    solve_prob: solve the problem
    '''        
    digit_list = read_input()

    return largest_consecutive_product(digit_list, 13)


if __name__ == '__main__':
    soln = solve_prob()
    print("Solution to Prob 8: {}".format(soln))
