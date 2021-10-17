# ------------------------------------------------------------------
# Problem 66
# ------------------------------------------------------------------
# Find D ≤ n (n = 1000 is the default) that has the largest x-value
# in the minimal solution to Pell's Equation:\n
# x^2 - D ⋅ y^2
#
# Solved: 2021-10-16
# ------------------------------------------------------------------

"""
Solves Pell's Equation
"""
function is_pell_solution(x::BigInt, y::BigInt, D::BigInt)
    return x^2 - D * y^2 == 1
end


"""
Convert the continued fraction to a rational
"""
function cont_frac_to_rational(as)
    n = length(as)
    a = as[n]

    for i = (n-1):-1:1
        a = as[i] + 1 // a
    end

    return a
end


"""
Solve Pell's Equation:\n
  x^2 - D ⋅ y^2
"""
function solve_pell_equation(D::BigInt)
    local x, y
    a = convert(BigInt, floor(√D))
    b = √D
    as = [a]

    while true
        # compute terms of the continued fraction convergent
        # for √D
        b = 1 / (b - floor(b))
        a = convert(BigInt, floor(b))
        b = b - a
        push!(as, a)

        # the minimial Pell solution is the numerator/denominator
        # x/y for the first continued fraction convergent for √D
        # where x/y solves the Pell equation... you can also check
        # minimal periodicity, but I didn't do it that way!
        f = cont_frac_to_rational(as)
        x = numerator(f)
        y = denominator(f)

        # have we got enough terms to get a solution?
        if is_pell_solution(x, y, D)
            break
        end
    end

    return (x, y)
end


"""
Find D ≤ n (n = 1000 is the default) that has the largest x-value
in the minimal solution to Pell's Equation:\n
    x^2 - D ⋅ y^2
"""
function solve_prob_0066(n = 1000)
    minimal_pell_soln = [
        (D, solve_pell_equation(big(D))) for D ∈ 2:n if floor(√D)^2 != D
    ]

    idx_x_max = argmax([x for (D,(x,y)) ∈ minimal_pell_soln])
    D_x_max, _ = minimal_pell_soln[idx_x_max]

    return D_x_max
end

D_with_largest_x = solve_prob_0066(1000)
println("Solution to Problem 66: $(D_with_largest_x)")
