matrix = matrix(c(6, 1, 1, 4, -2, 5, 2, 8, 7), nrow = 3, byrow = TRUE)

calc_determinant = function(matrix) {
    if (!is.matrix(matrix) || nrow(matrix) != ncol(matrix)) {
        stop("Error: The matrix is not a square matrix!")
    }
  
    n = nrow(matrix) 
  
    if (n == 1) {
        return(matrix[1, 1])
    } 
    if (n == 2) {
        return(matrix[1,1] * matrix[2,2] - matrix[1,2] * matrix[2,1])
    }
  
    determinant_value = 0
    for (j in 1:n) {
        sub_matrix = matrix[-1, -j]
        cofactor = (-1)^(1 + j) * calc_determinant(sub_matrix)
        determinant_value = determinant_value + matrix[1, j] * cofactor
    }
  
    return(determinant_value)
}

cat("Matrix:\n\n")
print(matrix)
cat("\nDeterminant = ", calc_determinant(matrix), "\n")