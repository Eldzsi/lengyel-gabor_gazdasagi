x = c(2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1)

binomial_test = function(x, y) {
    N = length(x)

    freq = 0
    for (i in 1:N) {
        if (x[i] == y) {
            freq = freq + 1
        }
    }

    p = freq/N
    m = N * p
    q = 1 - p
    deviation = N * p * q

    R = 0
    found = FALSE
    for (i in 1:N) {
        if (x[i] == y) {
            if (!found) {
                found = TRUE
                R = R + 1
            } 
        } else {
            if (found) {
                found = FALSE
            }
        }
    }

    return((R - m) / deviation)
}


Z = binomial_test(x, 2)
cat("Z =", Z, "\n")