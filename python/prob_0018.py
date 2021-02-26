'''
    Problem 18:
    Find the largest sum that can be obtained by moving from
    top to bottom by moving to adjacent entries on the rows
    below.

    Solved: 2014-08-21
'''


def get_input():
    with open('../data/prob_0018.txt', 'r') as f:
        triangle = []
        for line in f:
            triangle.append([int(x) for x in line.split()])

    return triangle


def find_max_sum(triangle):
    n_rows = len(triangle)
    max_sum_below = [[0 for e in row] for row in triangle]

    # initialize bottom row of sum to triangle
    max_sum_below[n_rows - 1] = triangle[n_rows - 1][:]

    # add up the max sum from adjacent elements below
    for i in range(n_rows-2, -1, -1):
        for j in range(0, i+1):
            max_sum_below[i][j] = triangle[i][j] + \
                max(max_sum_below[i+1][j], max_sum_below[i+1][j+1])

    return max_sum_below[0][0]


def solve_prob():
    return find_max_sum(get_input())


if __name__ == "__main__":
    rslt = solve_prob()
    print("Solution to Problem 18: {}".format(rslt))
