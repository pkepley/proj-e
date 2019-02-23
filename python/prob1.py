''' Problem 1:
    calculate the sum of all integers in [1,1000) 
    which satisfy 3|x or 5|x
'''

def dividesIt(x, d):
    return int(x/d) * d == x
    
if __name__ == "__main__":
    prob1_rslt = sum([x for x in range(1,1000) if (dividesIt(x,3) or dividesIt(x,5))])
    print("Problem 1: {}".format(prob1_rslt))
