g <- 4
n <- c(30, 25, 27, 18)
N <- sum(n)
r_means <- c(52, 45.3, 46.4, 61.4)

temp = 0
for (i in 1:g){
    temp = temp + n[i] * r_means[i]^2
}

H <- (12*temp)/(N*(N+1)) - 3*(N+1)
