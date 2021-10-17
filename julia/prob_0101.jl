# ------------------------------------------------------------------
# Problem 101
# ------------------------------------------------------------------
# Given a generating polynomial P of degree d, consider the sequence
# a_n = P(n), n = 1,2,...
#
# Let
# - Q_n be the polynomial of degree n-1 satisfying Q_n(k) = 1,...,n
# - FIT(n) be the smallest integer >= 1 for which Q_n(k) != a_n
#
# Find the sum of FIT(n) for the generating polynomial:
#     1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
#
# Solved: 2021-10-17
# ------------------------------------------------------------------

"""
Interpolate {(x, y) : x ∈ xs, y ∈ ys} via Newton's interpolation.
Returns the coefficients in Newton's form, NOT in standard form.
"""
function newton_interpolation(xs, ys)
    n = length(xs)

    M = zeros(n, n)
    M[:, 1] .= 1
    for j = 2:n
        M[j:n, j] = M[j:n, j-1] .* (xs[j:n] .- xs[j-1])
    end

    return M \ ys
end


"""
Evaluate the newton polynomial described by (xs,cs) at x
"""
function evaluate_newton(x, xs, cs)
    S = cs[1]
    p = 1

    for i = 2:length(cs)
        p = (x - xs[i-1]) * p
        S += cs[i] * p
    end

    return S
end


"""
Find the optimal polynomial (BOP) of degree k
for the generating polynomial P. Returns coefficients,
in Newton form.
"""
function find_op_poly(P, k)
    xs = collect(1:k)
    ys = map(P, xs)

    return newton_interpolation(xs, ys)
end


"""
Find the first incorrect term (FIT) for the
optimal polynomial of degree k for the
generating polynomial P
"""
function find_fit(P, k)
    xs = collect(1:k)

    # get coefficients, and convert them to integers
    cs = find_op_poly(P, k)
    cs = map(c -> convert(Int, c), cs)

    local n = 1
    local fit

    while true
        fit = evaluate_newton(n, xs, cs)

        if fit == P(n)
            n += 1
            continue
        else
            break
        end
    end

    return fit
end


"""
Sum FITS for BOPS, given generating poly P and its degree d.
"""
function sum_fits_for_bops(P, d)
    S = 0

    for k = 1:d
        S += find_fit(P, k)
    end

    return S
end


"""
Should evaluate to 74!
"""
function sanity_check_prob_0101()
    P = n -> n^3
    d = 3

    return 74 == sum_fits_for_bops(P, d)
end


"""
Find the sum of the FITS for the BOPS for the generating polynomial:
     1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
"""
function solve_prob_0101()
    P = n -> (1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10)
    d = 10

    return sum_fits_for_bops(P, d)
end


println("Solution to Problem 101: $(solve_prob_0101())")
