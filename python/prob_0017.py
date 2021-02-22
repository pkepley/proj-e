'''
    Problem 17:
    Compute the number of LETTERS in the english written
    versions of the positive integers less than or equal
    to 1000.
    (Note, 122 is considered one hundred and twenty two)

    Solved: 2014-08-21
'''


_nbr_name_tbl = {0: 'zero', 1: 'one', 2: 'two', 3: 'three', 4: 'four',
                 5: 'five', 6: 'six', 7: 'seven', 8: 'eight',
                 9: 'nine', 10: 'ten', 11: 'eleven', 12: 'twelve',
                 13: 'thirteen', 14: 'fourteen', 15: 'fifteen',
                 16: 'sixteen', 17: 'seventeen', 18: 'eighteen',
                 19: 'nineteen', 20: 'twenty', 30: 'thirty', 40: 'forty',
                 50: 'fifty', 60: 'sixty', 70: 'seventy', 80: 'eighty',
                 90: 'ninety', 100: 'hundred', 1000: 'thousand'}


def number_to_name(n):
    if 0 <= n < 20:
        return _nbr_name_tbl[n]

    elif 20 <= n < 100:
        ones_place = n % 10
        tens_place = (n - ones_place) % 100
        name = _nbr_name_tbl[tens_place]

        if ones_place > 0:
            name = name + "-" + number_to_name(ones_place)

        return name

    elif 100 <= n < 1000:
        hundreds_remainder = n % 100
        hundreds_place = (n - hundreds_remainder) // 100
        name = _nbr_name_tbl[hundreds_place] + " " + _nbr_name_tbl[100]

        if hundreds_remainder > 0:
            name = name + " and " + number_to_name(hundreds_remainder)

        return name

    elif n == 1000:
        return _nbr_name_tbl[1] + " " + _nbr_name_tbl[1000]

    else:
        raise NotImplementedError


def solve_prob():
    number_names_list = [number_to_name(n) for n in range(1, 1000+1)]

    return sum([len(s.replace(" ", "").replace("-", "")) for s in
                number_names_list])


if __name__ == '__main__':
    soln = solve_prob()
    print("Solution to Prob 17: {}".format(soln))
