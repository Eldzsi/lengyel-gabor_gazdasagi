x = c(483, 502, 498, 496, 502, 483, 494, 491, 505, 486)
m0 = 500
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

calc_sn = function(x, n) {
    sn = 0
    avg = calc_avg(x)

    for (i in x) {
        sn = sn + (i - avg)^2
    }

    return(sqrt( 1/(n-1) * sn ))
}

calc_t = function(x, m0) {
    return( (calc_avg(x) - m0) / (calc_sn(x, length(x)) / sqrt(length(x))) )
}

t_value = calc_t(x, m0)

cat("t =", t_value, "\n")
cat("Elfogadjuk H0-t?", is_number_in_interval(t_value, interval[1], interval[2]), "\n")