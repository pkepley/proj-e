'''
    Problem 14:
    Compute the longest Collatz sequence with starting value less than 10^6
    ie, if n even, next is n/2
        if n odd,  next is 3*n+1

    Solved: 2014-08-21
'''


def next_collatz(n):
    '''
    ---------------------------------------------------------------------------
    next_collatz
    ---------------------------------------------------------------------------
    Compute the next term in the Collatz sequence from n.
    '''
    if n % 2:
        return 3*n + 1
    else:
        return n//2


def collatz_sequence_length(n, collatz_memo_dict={1: 1}):
    '''
    ---------------------------------------------------------------------------
    collatz_sequence_length
    ---------------------------------------------------------------------------
    Compute the length of the Collatz sequence starting at n.

    You can optionally provide a lookup dictionary (collatz_memo_dict)
    with pre-computed sequence lengths for memoization.
    '''
    collatz_sequence = [n]
    next_term = next_collatz(n)

    # compute the Collatz sequence beginning from n until we
    # reach a term whose length is known
    while next_term not in collatz_memo_dict:
        collatz_sequence.append(next_term)
        next_term = next_collatz(next_term)

    # get the sequence length for the next term
    sequence_length = collatz_memo_dict[next_term]

    # use the known length to compute the sequence length for each
    # prior term
    for m in reversed(collatz_sequence):
        sequence_length += 1
        collatz_memo_dict[m] = sequence_length

    return collatz_memo_dict[n]


def longest_collatz_sequence(max_n, collatz_memo_dict={1: 1}):
    '''
    ---------------------------------------------------------------------------
    longest_collatz_sequence
    ---------------------------------------------------------------------------
    Find the longest Collatz sequence generated by an integer that is less than
    max_n. You can optionally provide a dictionary of

    You can optionally provide a lookup dictionary (collatz_memo_dict)
    with pre-computed sequence lengths for memoization.
    '''

    collatz_length_list = []

    for n in range(1, max_n):
        collatz_length_list.append(
            collatz_sequence_length(n, collatz_memo_dict)
        )

    # index is one less than the integer it represents, so add 1 (:
    return collatz_length_list.index(max(collatz_length_list)) + 1


def solve_prob(n=10**6):
    return longest_collatz_sequence(n)


if __name__ == "__main__":
    rslt = solve_prob()
    print("Solution to Problem 14: {}".format(rslt))
