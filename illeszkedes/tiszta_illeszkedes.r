k = c(11, 21, 13, 32, 13, 10)
p = rep(1/6, 6)

chi_square = function(k, p, N) {
    if (length(k) == 0) {
        stop("Error: Invalid k vector!")
    }

    if (length(p) == 0) {
        stop("Error: Invalid p vector!")
    }

    if (length(k) != length(p)) {
        stop("Error: The lengths of the two vectors must be the same!")
    }

    N = 0
    for (i in 1:length(k)) {
        N = N + k[i]
        if (k[i] < 0) {
            stop("Error: Negative value in k vector!")
        }

        if (p[i] < 0) {
            stop("Error: Negative value in p vector!")
        }
    }

    s = 0
    for (i in 1:length(k)) {
        s = s + (((k[i] - N * p[i]))^2) / (N * p[i])
    }

    return(s)
}

result = chi_square(k, p, N)
cat("Result:", toString(result), "\n")

if (result < qchisq(0.95, length(k)-1)) {
    print("Elfogadjuk H0-t.")
} else {
    print("Elutasítjuk H0-t.")
}