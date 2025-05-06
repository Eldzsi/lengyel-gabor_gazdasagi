x = c(1, 2, 3, 4, 5)
y = c(0.8, 1.4, 1.9, 2.7, 3.8)

# y = ax + b
linear_regression = function(x, y) {
    n = length(x)

    a = (n * sum_product(x, y) - sum_vector(x) * sum_vector(y)) / (n * sum_product(x, x) - sum_vector(x)^2)
    b = (sum_product(x, x) * sum_vector(y) - sum_product(x, y) * sum_vector(x)) / (n * sum_product(x, x) - sum_vector(x)^2)

    print(paste("x =", toString(x)))
    print(paste("y =", toString(y)))

    print(paste("a =", a))
    print(paste("b =", b))
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

