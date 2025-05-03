x = c(50380, 46237, 23539, 41897, 19570, 52646, 15601, 16470, 46473, 82484)
y = c(5500, 5030, 5020, 4790, 4790, 4810, 5710, 3380, 5550, 5390)

linear_regression = function(x, y) {
    n = length(x)

    a = (n * sum_product(x, y) - sum_vector(x) * sum_vector(y)) / (n * sum_product(x, x) - sum_vector(x)^2)
    b = (sum_product(x, x) * sum_vector(y) - sum_product(x, y) * sum_vector(x)) / (n * sum_product(x, x) - sum_vector(x)^2)

    cat("x =", toString(x), "\n")
    cat("y =", toString(y), "\n")
    cat("a =", a, "\n")
    cat("b =", b, "\n")
}

sum_vector = function(x) {
    if (length(x) == 0) {
        stop("Error: The vector is empty!")
    }
    
    total = 0
    for (i in 1:length(x)) {
        total = total + x[i]
    }

    return(total)
}

sum_product = function(x, y) {
    if (length(x) == 0 || length(y) == 0) {
        stop("Error: One or both vectors are empty!")
    }

    if (length(x) != length(y)) {
        stop("Error: Vectors must have the same length!")
    }

    total = 0
    for (i in 1:length(x)) {
        total = total + x[i] * y[i]
    }

    return(total)
}

linear_regression(x, y)

