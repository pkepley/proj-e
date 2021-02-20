''' Problem 2:
    Find the sum of the terms in the Fibonacci sequence (F_n)
    that  satisfy
        1) F_n < 4 * 10^6
        2) F_n is even

   Solved: 2014-01-28
'''


# compute the terms of the Fibonacci sequence up to n
def evenFibs(n):
    a, b = 1, 2
    evens = []

    while(b < n):
        a, b = b, a+b

        if a % 2 == 0:
            evens.append(a)

    return evens


def solve_prob_2():
    return sum(evenFibs(4 * 10**6))


if __name__ == '__main__':
    # sum the terms which are even, up to 4 * 10^6
    prob_2_rslt = solve_prob_2()
    print("Solution to Problem 2: {0}".format(prob_2_rslt))
