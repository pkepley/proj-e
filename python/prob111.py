import itertools
from PrimeTools import PrimeTools

# start out with a sieve up to 10**5
_pt_ = PrimeTools(10**5)

def kbits(n, k):
    '''
    ---------------------------------------------------------------------
    kbits(n,k)
    ---------------------------------------------------------------------
    Generate list of all n-bit bit-patterns with exactly k bits set to 1.
    '''
    # https://stackoverflow.com/questions/1851134/generate-all-binary-strings-of-length-n-with-k-bits-set
    result = []
    for bits in itertools.combinations(range(n), k):
        s = [ 0 ] * n
        for bit in bits:
            s[bit] = 1
        result.append(s)
    return result

def int_from_digits(digits):
    '''
    ---------------------------------------------------------------------
    int_from_digits(digits)
    ---------------------------------------------------------------------
    Convert a list of digits to an integer.
    '''
    n = 0
    for i, d in enumerate(reversed(digits)):
        n += d * 10**(i)
    return n


def M(n, d, pt = _pt_):
    '''
    ---------------------------------------------------------------------
    M(n, d):
    ---------------------------------------------------------------------
    The maximum number of times that d appears in an n digit prime.
    '''

    # non-d digits:
    other_digits = [i for i in range(10) if i != d]

    for k in range(n - 1, 0, -1):
        d_test_patterns = kbits(n, k)

        for d_pattern in d_test_patterns:           
            for non_d_digit_list in itertools.product(other_digits, repeat = n - k):
                m_test_digits = [0 for dd in range(n)]

                i = 0
                for l in range(n):
                    if d_pattern[l] == 1:
                        m_test_digits[l] = d
                    else:
                        m_test_digits[l] = non_d_digit_list[i]
                        i += 1
                        
                if m_test_digits[0] != 0:
                    m_test = int_from_digits(m_test_digits)

                    if pt.isPrime(m_test):
                        return k


def N_set(n, d, pt = _pt_):
    '''
    ---------------------------------------------------------------------
    N_set(n, d):
    ---------------------------------------------------------------------
    The set of all n digit primes, with d repeated M(n, d) times
    '''    
    
    # compute M(n,d)
    M_n_d = M(n, d, pt)

    # list to keep track of all primes achieving M(n,d)
    N_set = []

    # list of non-d digits
    other_digits = [i for i in range(10) if i != d]
    
    # possible locations for d
    d_test_patterns = kbits(n, M_n_d)

    # check through all possible locations for d...
    for d_pattern in d_test_patterns:

        # in all other places, we will put all other non-d digits...
        for non_d_digit_list in itertools.product(other_digits, repeat = n - M_n_d):
            
            # generate the digits for the test integer m_test:
            m_test_digits = [0 for dd in range(n)]
            i = 0
            for l in range(n):
                if d_pattern[l] == 1:
                    m_test_digits[l] = d
                else:
                    m_test_digits[l] = non_d_digit_list[i]
                    i += 1

            # digit 0 cannot be 0, otherwise too short:
            if m_test_digits[0] != 0:
                m_test = int_from_digits(m_test_digits)

                # if prime, add to list:
                if pt.isPrime(m_test):
                    N_set.append(m_test)
                
    return N_set

def N(n, d, pt = _pt_):
    '''
    ---------------------------------------------------------------------
    Compute N(n, d):
    ---------------------------------------------------------------------
    The number of n digit primes, with d repeated M(n, d) times.
    '''    
    return len(N_set(n, d, pt))

def S(n, d, pt = _pt_):
    '''
    ---------------------------------------------------------------------
    Compute S(n, d):
    ---------------------------------------------------------------------
    The sum over all primes with n digits, with d repeated M(n, d) times.
    '''
    return sum(N_set(n, d, pt))

def solve_prob_111(n, pt = _pt_):
    S_set = []
    for d in range(10):
        S_set.append(S(n,d))
        
    return sum(S_set)

if __name__ == "__main__":
    n = 10
    print("d\tM(n,d)\tN(n,d)\tS(n,d)")
    print("--------------------------------")
    for d in range(10):
         print("{0}\t{1}\t{2}\t{3}".format(d, M(n, d), N(n, d), S(n,d)))

    prob_111_soln = solve_prob_111(n)
    
    print("Solution to Prob 111: {}".format(prob_111_soln))

    
