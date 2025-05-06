k = c(20, 45, 101, 132, 224, 190, 156, 87, 41, 4)
c_values = c(-2.0, -1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5, 2.0)

normal_distribution_test = function() {
    if (length(k) == 0) {
        stop("Error: Invalid k vector length!")
    }

    for (i in 1:length(k)) {
        if (k[i] < 0) {
            stop("Error: Negative value in k vector!")
        }
    }

    if (length(c_values) == 0) {
        stop("Error: Invalid c_values vector length!")
    }

    if (length(c_values) != length(k)-1) {
        stop("Error: Invalid vector lengths!")
    }

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

    return(khi)
}

result = normal_distribution_test()
cat("khi =", khi, "\n")

if (result < qchisq(0.95, length(k)-1)) {
    print("Elfogadjuk H0-t.")
} else {
    print("Elutasítjuk H0-t.")
}