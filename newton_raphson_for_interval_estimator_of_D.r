number_of_observations <- 200
alpha <- 0.05
a0 <- qchisq(alpha / 2, df = number_of_observations - 1)
b0 <- qchisq(1 - alpha / 2, df = number_of_observations - 1)
u0 <- a0 / b0
u_running <- u0
# Newton - Raphson method. u_(n+1) = u_n - f(u_n)/f'(u_n)
tolerance <- 5 * 10^(-5)
count <- 0
running <- TRUE
while (running){
    count <- count + 1
    old_u <- u_running
    v <- log(u_running)
    var1 <- (number_of_observations + 1) * v / (u_running - 1)
    var2 <- var1 * u_running
    denominator <- (number_of_observations + 1) * ((-v / (u_running - 1)^2 + 1 / (u_running * (u_running - 1))) * dchisq(var1, number_of_observations - 1) - (-v / (u_running - 1)^2 + 1 / (u_running - 1)) * dchisq(var2, number_of_observations - 1)) 
    numerator <- pchisq(var1, number_of_observations - 1) - pchisq(var2, number_of_observations - 1) - 1 + alpha
    if (denominator == 0){
        break()
    }
    u_running <- u_running - numerator / denominator
    if (any(abs(old_u - u_running) < tolerance, count > 20)){
        break()
    }
}
print(u_running)
print(var1 - b0)
print(var2 - a0)
# Conclusion: the difference is relatively large if number_of_observations is small, and vice versa.