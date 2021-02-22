'''
    Problem 15:
    Compute the number of paths in a 20 x 20 grid
    where paths begin in the upper left hand
    corner and can only move to the right or down.

    Solved: 2014-08-21
'''


def n_paths_right_down(width, height):
    '''
    ---------------------------------------------------------------------------
    n_paths_right_down
    ---------------------------------------------------------------------------
    Compute the number of paths that can be made moving only right and down
    from the top left corner to the bottom right corner in a grid with
    width x height points.
    '''
    # number of paths down and right from the from the (i,j) entry
    n_paths = [[0 for j in range(0, width+1)] for i in range(0, height+1)]

    # fill the lower half-grid
    for k in range(height, -1, -1):
        i, j = k, width

        while 0 <= i <= height and 0 <= j <= width:
            if i == height or j == width:
                n_paths[i][j] = 1
            else:
                n_paths[i][j] = n_paths[i][j+1] + n_paths[i+1][j]
            i, j = i+1, j-1

    # fill the upper half-grid
    for k in range(width, -1, -1):
        i, j = 0, k

        while 0 <= i <= height and 0 <= j <= width:
            if i == height or j == width:
                n_paths[i][j] = 1
            else:
                n_paths[i][j] = n_paths[i][j+1] + n_paths[i+1][j]
            i, j = i+1, j-1

    return n_paths[0][0]


def solve_prob(width=20, height=20):
    return n_paths_right_down(width, height)


if __name__ == "__main__":
    rslt = solve_prob()
    print("Solution to Problem 15: {}".format(rslt))
