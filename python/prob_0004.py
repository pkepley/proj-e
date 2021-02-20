'''
    Problem 4:
    Find the largest palindromic number that is a product
    of two three digit numbers.

    Solved: 2014-11-18
'''


def is_palindrome(n):
    n_as_str = str(n)
    n_as_str_len = len(n_as_str)

    for k in list(range(0, n_as_str_len // 2)):
        if not n_as_str[k] == n_as_str[n_as_str_len-k-1]:
            return False

    return True


def find_largest():
    search_range = list(range(100, 1000))
    first_factor = 999
    palindromes_found = []

    while(first_factor > 99):
        for x in search_range:
            prod = first_factor * x

            if is_palindrome(prod):
                palindromes_found.append(prod)

        first_factor -= 1
        search_range.pop()

    palindromes_found.sort()

    return palindromes_found[len(palindromes_found) - 1]


def solve_prob_4():
    return find_largest()


if __name__ == '__main__':
    prob_4_rslt = solve_prob_4()
    print("Solution to Problem 4: {0}".format(prob_4_rslt))
