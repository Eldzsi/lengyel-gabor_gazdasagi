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

calc_t = function(x, m0) {
    return( (calc_avg(x) - m0) / (sd / sqrt(length(x))) )
}