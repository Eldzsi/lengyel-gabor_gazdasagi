x = c(14.75, 19.31, 17.83, 16.91, 15.23, 18.15, 19.85, 17.32, 18.39, 15.74, 16.84, 16.53, 20.16, 17.72, 18.70, 23.27, 16.94, 18.64, 20.95, 21.99)
y = c(17.56, 20.98, 19.87, 19.18, 17.93, 20.11, 21.39, 19.49, 20.30, 18.31, 19.13, 18.90, 21.62, 19.79)

sigma1 = 2
sigma2 = 1.5

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