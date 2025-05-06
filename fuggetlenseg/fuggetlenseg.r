k = matrix(c(15, 10, 5, 10, 10, 20, 5, 20, 5), nrow = 3, byrow = TRUE)

test_of_independence = function(k) {
    if (ncol(k) == 0 || nrow(k) == 0) {
        stop("Error: Invalid k matrix!")
    }

    N = sum(k)
    khi = 0
    df = (nrow(k)-1) * (ncol(k)-1)
    row_sum = rowSums(k)
    col_sum = colSums(k)

    for (i in 1:nrow(k)) {
        for (j in 1:ncol(k)) {
            khi = khi + ((k[i, j] - (row_sum[i] * col_sum[j] / N))^2 / ((row_sum[i] * col_sum[j]) / N))
        }
    }

    p = 1 - pchisq(khi, df)

    cat("row_sum =", row_sum, "\n")
    cat("col_sum =", col_sum, "\n")
    cat("N =", N, "\n")
    cat("DF =", df, "\n")
    cat("Khi =", khi, "\n")
    cat("p =", p, "\n")

    if (p < 0.05) {
        cat("Nem fuggetlenek.\n")
    } else {
        cat("Fuggetlenek.\n")
    }
}

test_of_independence(k)

# print(chisq.test(k))