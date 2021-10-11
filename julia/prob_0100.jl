# ------------------------------------------------------------------
# Problem 100
# ------------------------------------------------------------------
# Some arrangements of urns (filled with red and blue marbles)
# have the property that P(BB) = 0.5. Find the number of blue
# marbles in the smallest such urn containing > 10^12 balls.
#
# Solved: 2021-10-10
# ------------------------------------------------------------------

function solve_prob_0100(n)
    # Brute forced solutions for n < 10^10 in Haskell / C++
    # only to have to give up and check out OEIS. Turns out the
    # series of Blue & Red marbles satisfy simple recurrence relations.

    # Blue series is: A011900
    prev_blue, curr_blue = 1, 3

    # Red series is: A001109
    prev_red, curr_red = 0, 1

    while (curr_blue + curr_red) < n
        # a[n] = 6 * a[n-1] - a[n-2] - 2
        curr_blue, prev_blue = 6 * curr_blue - prev_blue - 2, curr_blue

        # a[n] = 6 * a[n-1] - a[n-2]
        curr_red, prev_red = 6 * curr_red - prev_red, curr_red
    end

    return curr_blue
end

n_blue = solve_prob_0100(10^12)
println("Solution to Problem 100: $(n_blue)")
