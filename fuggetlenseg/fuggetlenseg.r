k = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)

main = function(k) {
    if (ncol(k) == 0 || nrow(k) == 0) {
        stop("Error: Invalid k matrix!")
    }

    N = 0
    khi = 0
    df = (nrow(k)-1) * (ncol(k)-1)

    cat("DF =", df, "\n")

    for (i in 1:nrow(k)) {
        for (j in 1:ncol(k)) {
            N = N + k[i, j]
        }
    }

    cat("N =", N, "\n")

    for (i in 1:nrow(k)) {
        for (j in 1:ncol(k)) {
        }
    }
}

main(k)