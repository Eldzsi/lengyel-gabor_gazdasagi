x = c(4.2, 6.1, 5.3, 6.8, 3.9)
y = c(4.6, 7.1, 5.8, 6.5)

sigma1 = 2.4
sigma2 = 3.2

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

calc_u_two_sample = function(x, y, sigma1, sigma2) {
    return((calc_avg(x) - calc_avg(y)) / sqrt((sigma1^2 / length(x)) + (sigma2^2 / length(y))))
}

u_value = calc_u_two_sample(x, y, sigma1, sigma2)

cat("u =", u_value, "\n")
cat("Elfogadjuk H0-t?", is_number_in_interval(u_value, interval[1], interval[2]), "\n")