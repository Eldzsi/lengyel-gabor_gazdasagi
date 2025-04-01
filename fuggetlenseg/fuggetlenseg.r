k = matrix(c(15, 10, 5, 10, 10, 20, 5, 20, 5), nrow = 3, byrow = TRUE)

main = function(k) {
    if (ncol(k) == 0 || nrow(k) == 0) {
        stop("Error: Invalid k matrix!")
    }

    N = sum(k)
    khi = 0
    df = (nrow(k)-1) * (ncol(k)-1)

    cat("DF =", df, "\n")
    cat("N =", N, "\n")

    row_sums = rowSums(k)
    col_sums = colSums(k)

    for (i in 1:nrow(k)) {
        for (j in 1:ncol(k)) {
            expected = (row_sums[i] * col_sums[j]) / N
            khi = khi + ((k[i, j] - expected)^2 / expected)
        }
    }

    p = 1 - pchisq(khi, 4)

    cat("Khi =", khi, "\n")
    cat("p =", p, "\n")

    if (p < 0.05) {
        cat("Nem fuggetlenek\n")
    } else {
        cat("Fuggetlenek.)\n")
    }
}

main(k)