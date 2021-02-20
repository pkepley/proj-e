'''
   Problem 11:
   Given a 20x20 grid (saved in ../data/prob_0011.txt) of numbers,
   find the greatest product of four adjacent entries, where
   adjacency means in a row, in a column, or diagonally adjacent.

'''

from operator import mul
from functools import reduce


def maxProd(grid, nAdj):
    '''
    maxProd:
    -----------------------------------------------------------
    computes the product of nAdj adjacent entries from a
    grid which is input as a list of rows, each with a constant
    number of columns. Needless to say, nAdj must be
    less than min(nGridRows, nGridCols), where nGridRows is the
    number of rows in the grid and nGridCols is the number of
    columns in the grid. Checks in row, col, sw-to-ne-diags and
    nw-to-se-diags.
    '''

    mxPrdFnd = 0
    nRows = len(grid)
    nCols = len(grid[0])  # all rows assumed to have same nCols

    # check in rows
    for row in grid:
        for i in range(0, nCols - nAdj):
            curPrd = reduce(mul, row[i:(i+nAdj)], 1)
            if curPrd > mxPrdFnd:
                mxPrdFnd = curPrd

    # check in columns
    for i in range(0, nRows - nAdj):
        for j in range(0, nCols):
            curPrd = reduce(mul, [grid[i+m][j] for m in range(0, nAdj)], 1)
            if curPrd > mxPrdFnd:
                mxPrdFnd = curPrd

    # check along nw-to-se-diags
    for i in range(0, nRows - nAdj):
        for j in range(0, nCols - nAdj):
            curPrd = reduce(mul, [grid[i+m][j+m] for m in range(0, nAdj)], 1)
            if curPrd > mxPrdFnd:
                mxPrdFnd = curPrd

    # check along ne-to-sw-diags
    for i in range(nAdj, nRows):
        for j in range(0, nCols - nAdj):
            curPrd = reduce(mul, [grid[i-m][j+m] for m in range(0, nAdj)], 1)
            if curPrd > mxPrdFnd:
                mxPrdFnd = curPrd

    return mxPrdFnd


def readGrid():
    theFile = open('../data/prob_0011.txt', 'r')
    grid = []

    for line in theFile:
        row = line.split()
        row = list(map(lambda x: int(float(x)), row))
        grid.append(row)

    theFile.close()
    return grid


def solve_prob_11():
    return maxProd(readGrid(), 4)


if __name__ == '__main__':
    prob_11_rslt = solve_prob_11()
    print("Solution to Problem 11: {}".format(prob_11_rslt))
