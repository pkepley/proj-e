'''
    Problem 19:
    How many Sundays fell on the first of the month during the
    twentieth century (1 Jan 1901 to 31 Dec 2000)?

    Solved: 2015-02-16
'''


# "local" global variables
_days_in_months = {1: 31, 2: 28, 3: 31, 4: 30, 5: 31, 6: 30,
                   7: 31, 8: 31, 9: 30, 10: 31, 11: 30, 12: 31}
_day_dict = {1: 'Mon', 2: 'Tues', 3: 'Weds', 4: 'Thurs', 5: 'Fri', 6: 'Sat',
             7: 'Sun', 0: 'Sun'}


def is_leap_year(year):
    '''A leap year occurs on any year evenly divisible by 4, but not on a
    century unless it is divisible by 400.'''
    return ((year % 4) == 0) and ((year % 100 != 0) or (year % 400 == 0))


def day_of_week(day, month, year):
    # 01/01/1900 was a MON
    week_day = 1
    n_years_since_1900 = year - 1900
    n_leap_years_since_1900 = len([y for y in range(1900, year) if
                                   is_leap_year(y)])

    # each year has at least 365 days, so the day of 01/01 will cycle
    # by 1 for each year
    week_day = (week_day + n_years_since_1900) % 7

    # have to add an additional day for each leap year in between
    week_day = (week_day + n_leap_years_since_1900) % 7

    # add days for the month
    week_day = week_day + sum([_days_in_months[i] for i in range(1, month)])
    week_day = week_day % 7

    # gain a day past 29-Feb in leap years
    if is_leap_year(year) and (month > 2):
        week_day = (week_day + 1) % 7

    # add days for the day in the month
    week_day = (week_day + day - 1) % 7

    return _day_dict[week_day]


def solve_prob():
    # compute the number of months beginning on Sundays
    # in each calendar year from 1901 to 2000 inclusive
    first_day_of_months = [day_of_week(1, mm, yy) for mm in range(1, 12+1)
                           for yy in range(1901, 2001)]

    return len([d for d in first_day_of_months if d == 'Sun'])


if __name__ == '__main__':
    rslt = solve_prob()
    print("Solution to Problem 19: {}".format(rslt))
