# ------------------------------------------------------------------
# Problem 144
# ------------------------------------------------------------------
# Consider a laser beam bouncing around inside of an ellipse with 
# a small slit. How many times does the beam reflect before 
# escaping the ellipse?
# 
# Solved: 2021-10-11
# ------------------------------------------------------------------

# compute where p + t * v intersects the ellipse given by
#     c1 * x^2 + c2 * y^2 = c3
# from the positive sense of v
function point_ellipse_intersection(p, v, c1=4, c2=1, c3=100)
    # we solve the quadratic equation for t:
    #   c1*(p[1] + t*v[1])^2 + c2 * (p[2] + t*v[2])^2 = c3

    a = c1*v[1]^2 + c2*v[2]^2
    b = 2*(c1*p[1]*v[1] + c2*p[2]*v[2])
    c = c1*p[1]^2 + c2*p[2]^2 - c3

    # two solutions to quadratic equation
    t1 = (-b - sqrt(b^2 - 4*a*c)) / (2.0*a)
    t2 = (-b + sqrt(b^2 - 4*a*c)) / (2.0*a)

    # we want the positive orientation
    if (t2 > 0)
        t = t2
    else
        t = t1
    end

    return p .+ (t .* v)
end


# compute the outward normal to the ellipse given by
#     c1 * x^2 + c2 * y^2 = c3
# at p on the ellipse
function outer_unit_normal(p, c1=4, c2=1, c3=100)
    N = [2*c1*p[1], 2*c2*p[2]]
    N = N ./ √(N[1]^2 + N[2]^2)

    return N
end

# reflect v at the point p
function compute_reflection(p, v, c1=4, c2=1, c3=100)
    N = outer_unit_normal(p, c1, c2, c3)

    # decompose v into perp and parallel components to N
    v_dot_N = v[1] * N[1] + v[2] * N[2]
    v_parallel = v_dot_N .* N
    v_perp = v - v_parallel

    # reflect v by reversing sign of v_perp
    v_refl = v_perp - v_parallel

    return v_refl
end


function count_reflections(p, v; max_bounces=Inf, c1=4, c2=1, c3=100,
                           exit_callback=(p -> false), keep_history=false)
    ps = [p]
    vs = [v]

    # how many bounces?
    n_bounces = 0

    # continue until we run out of bounces, or until
    # we trigger the exit_callback
    # ignore the exit_callback if it is the first iteration
    while (n_bounces < max_bounces) && ((n_bounces == 0) || !(exit_callback(p)))
        # compute next intersection point
        p = point_ellipse_intersection(p, v, c1, c2, c3)

        # compute reflection
        v = compute_reflection(p, v)

        # keep track of bounces
        if keep_history
            push!(ps, p)
            push!(vs, v)
        end

        # increment the bounce counter
        n_bounces += 1
    end

    if keep_history
        ps = hcat(ps...)
        vs = hcat(vs...)
    else
        ps = []
        vs = []
    end

    n_bounces -= 1

    return (n_bounces, ps, vs)
end


function solve_prob_0144()
    # first two locations specified
    p0 = [0.0, 10.1]
    p1 = [1.4, -9.6]

    # initial orientation is direction from p0 -> p1
    v0 = p1 .- p0

    # count bounces until exit condition is satisfied
    n_bounces, ps, vs = count_reflections(
        p0, v0,
        c1=4, c2=1, c3=100,
        exit_callback = p -> abs(p[1]) < 0.01 && p[2] > 0,
        keep_history=false
    )

    return n_bounces
end


# run if called as a script
if abspath(PROGRAM_FILE) == @__FILE__
    n_bounces = solve_prob_0144()
    println("Solution to Problem 144: $(n_bounces)")
end
