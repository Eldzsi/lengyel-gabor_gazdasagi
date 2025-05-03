x = c(1, 2, 3, 4)
y = c(5, 2.3, 3.2, 7)
x0 = 6

lagrange_interpolation = function(x, y, x0) {
    n = length(x)
    L = numeric(n)
  
    for (i in 1:n) {
        L[i] = 1
        for (j in 1:n) {
        if (i != j) {
            L[i] = L[i] * (x0 - x[j]) / (x[i] - x[j])
        }
        }
    }
  
    y0 = sum(y * L)
    return(y0)
}

result = lagrange_interpolation(x, y, x0)
print(result)