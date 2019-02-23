''' Problem 2:
Find the sum of the terms in the Fibonacci sequence (F_n)
which satisfy 
    1) F_n < 4 * 10^6
    2) F_n is even
'''

# compute the terms of the Fibonacci sequence up to n
def evenFibs(n):
    a,b = 1,2
    evens = []
    while(b < n):
        a, b = b, a+b
        if a % 2 == 0:
            evens.append(a)
    return evens

if __name__ == "__main__":
    # sum the terms which are even, up to 4 * 10^6
    prob2_rslt = sum(evenFibs(4000000))
    print("Problem 2: {}".format(prob2_rslt))

