#=
------------------------------------------------------
                      Problem 1
------------------------------------------------------
Find the sum of all the multiples of 3 or 5 BELOW 1000.
=#

# Slow, literal solution
function solve_prob_1(n_max)
    sum = 0
    
    for i = 3:n_max
        if rem(i, 3) == 0
            sum = sum + i
        elseif rem(i, 5) == 0
            sum = sum + i
        end
    end

    return sum
end

# Solve Problem 1
println(solve_prob_1(1000 - 1))
