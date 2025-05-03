x = c(105)
sigma = 16
m0 = 100
interval = c(-1.96, 1.96)

calc_avg = function(x) {
    if (length(x) == 0) {
        stop("Error: The vector is empty!")
    }

    total = 0
    for (i in x) {
        total = total + i
    }
    return(total / length(x))
}

is_number_in_interval = function(x, lower, upper) {
    if (upper <= lower) {
        stop("Error: Invalid range")
    }
    return(x >= lower && x <= upper)
}

calc_u = function(x, m, sigma) {
    n = length(x)
    return((calc_avg(x) - m) / (sigma / sqrt(n)))
}

u_value = calc_u(x, m0, sigma)

cat("u =", u_value, "\n")
cat("Elfogadjuk H0-t?", is_number_in_interval(u_value, interval[1], interval[2]), "\n")