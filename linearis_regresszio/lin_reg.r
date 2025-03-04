x = c(1, 2, 3, 4)
y = c(1.1, 1.9, 3, 4.1)

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

n = length(x)

a = (n * sum_product(x, y) - sum_vector(x) * sum_vector(y)) / (n * sum_product(x, x) - sum_vector(x)^2)
b = (sum_product(x, x) * sum_vector(y) - sum_product(x, y) * sum_vector(x)) / (n * sum_product(x, x) - sum_vector(x)^2)

cat("x =", toString(x), "\n")
cat("y =", toString(y), "\n")
cat("a =", a, "\n")
cat("b =", b, "\n")
