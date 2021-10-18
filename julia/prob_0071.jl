# ------------------------------------------------------------------
# Problem 71
# ------------------------------------------------------------------
# Find the reduced fraction that is to the "left" of n/d = (3/7)
# with m <= max_denominator.
#
# Solved: 2021-10-17
# ------------------------------------------------------------------


"""
Find the reduced fraction that is to the "left" of n/d = (3/7)
with m <= max_denominator.
"""
function fraction_to_the_left(n = 3, d = 7, max_denominator = 1_000_000)
    # best approx to n/d from the left
    best_approx = 0

    # approximate 3/7 with a fraction with denominator m
    for m = 1:max_denominator
        l = convert(Int, floor(3 * m / d))
        curr_approx  = l // m

        # is this approximation better than any we've seen?
        # and is the fraction strictly to the left?
        if curr_approx > best_approx && curr_approx < n//d
            best_approx = curr_approx
        end
    end

    return best_approx
end

"""
"""
function solve_prob_0071(n = 3, d = 7, max_denominator = 1_000_000)
    f_left = fraction_to_the_left(n, d, max_denominator)

    return numerator(f_left)
end

@assert numerator_to_left = solve_prob_0071(3, 7, 8) == 2

numerator_to_left = solve_prob_0071()
println("Solution to Problem 71: $(numerator_to_left)")
