k = c(20, 45, 101, 132, 224, 190, 156, 87, 41, 4)
c_values = c(-2.0, -1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5, 2.0)

main = function(k) {
    N = 0
    for (i in 1:length(k)) {
        N = N + k[i]
    }

    cat("N =", N, "\n")

    khi = 0
    for (i in 1:length(k)) {
        p = 0
        if (i == 1) {
            p = pnorm(c_values[i])
        } else if (i == length(k)) {
            p = 1 - pnorm(c_values[i - 1])
        } else {
            p = pnorm(c_values[i]) - pnorm(c_values[i - 1])
        }

        cat(sprintf("p%d = %.4f\n", i, p))

        khi = khi + (k[i] - p * N)^2 / (p * N) 
    }

    cat("khi =", khi, "\n")

}

main(k)