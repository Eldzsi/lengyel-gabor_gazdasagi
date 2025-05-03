x = c(52, 57, 62, 55, 64, 57, 56, 55)
y = c(41, 34, 33, 36, 40, 25, 31, 37, 34, 30, 38)
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

    print( 1 / (n - 1) * sn )
    return( 1 / (n - 1) * sn )
}

calc_t = function(x, y) {
    x_avg = calc_avg(x)
    y_avg = calc_avg(y)
    n1 = length(x)
    n2 = length(y)
    sn1 = calc_sn(x, n1)
    sn2 = calc_sn(y, n2)

    print(" ")
    print(x_avg)
    print(y_avg)
    print(n1-1)
    print(sn1)
    print(n2-1)
    print(sn2)
    print(" ")

    numerator = x_avg - y_avg
    denominator = sqrt((n1 - 1) * sn1 + (n2 - 1) * sn2)
    multiplier = sqrt((n1 * n2 * (n1 + n2 - 2)) / (n1 + n2))

    return(numerator / denominator * multiplier)
}

t_value = calc_t(x, y)

cat("t =", t_value, "\n")
cat("Elfogadjuk H0-t?", is_number_in_interval(t_value, interval[1], interval[2]), "\n")