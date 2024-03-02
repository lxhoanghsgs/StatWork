number_of_observations <- 12
alpha <- number_of_observations
confidence_prob <- 0.05
theta <- 1
u0 <- 0.5
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
    denominator <- (number_of_observations + 1) * ((-v / (u_running - 1)^2 + 1 / (u_running * (u_running - 1))) * dgamma(var1, shape = alpha, rate = theta) - (-v / (u_running - 1)^2 + 1 / (u_running - 1)) * dgamma(var2, shape = alpha, rate = theta)) 
    numerator <- pgamma(var1, shape = alpha, rate = theta) - pgamma(var2, shape = alpha, rate = theta) - 1 + confidence_prob
    if (denominator == 0){
        break()
    }
    u_running <- u_running - numerator / denominator
    if (any(abs(old_u - u_running) < tolerance, count > 20)){
        break()
    }
}
print(u_running)
print(var1)
print(var2)