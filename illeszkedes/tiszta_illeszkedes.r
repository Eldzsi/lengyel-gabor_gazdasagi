N = 100
k = c(11, 21, 13, 32, 13, 10)
p = rep(1/6, 6)

chi_square = function(k, p, N) {
    if (length(k) == 0) {
        stop("Error: Invalid k vector!")
    }

    if (length(p) == 0) {
        stop("Error: Invalid p vector!")
    }

    if (N < 1) {
        stop("Error: Invalid N valur!")
    }

    for (i in 1:length(k)) {
        if (k[i] < 0) {
            stop("Error: Invalid k vector!")
        }
         if (p[i] < 0) {
            stop("Error: Invalid p vector!")
        }
    }

    s = 0
    for (i in 1:length(k)) {
        s = s + (((k[i] - N * p[i]))^2) / (N * p[i])
    }
    
    return(s)
}

result = chi_square(k, p, N)
cat("Result: ", toString(result), "\n")
cat("DF:", length(k)-1, "\n")

if (result < 11.1) {
    print("Elfogadjuk")
} else {
    print("Elutasítjuk")
}


