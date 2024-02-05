# Use simulations to test the built-in chi-squared dist versus the empirical (by def.) chi-squared.
library(MASS)
count_for_cdf <- function(dataset, upper_bound){
    # Counts the number of elements in dataset that are not bigger than upper_bound.
    count <- 0
    for (i in dataset){
        if (i <= upper_bound){
            count <- count + 1
        }
    }
    return(count)
}
cdf_empirical_chi <- function(deg_of_freedom, number_of_observations){
    # Returns a plot of the empirical cdf (cumulative distribution function) of chi-squared distribution.
    empirical_chi <- vector(mode = "integer", length = number_of_observations)
    for (i in 1:number_of_observations){
        x_normal <- mvrnorm(n = 1, vector(mode = "integer", length = deg_of_freedom), Sigma = diag(deg_of_freedom))
        empirical_chi[i] <- t(x_normal) %*% x_normal
    }
    checkpoints <- seq(0, 7 * deg_of_freedom, by = 0.05)
    empirical_cdf <- vector(mode = "integer", length = length(checkpoints))
    for (i in 1:length(empirical_cdf)){
        empirical_cdf[i] <- count_for_cdf(empirical_chi, checkpoints[i])/number_of_observations
        if(empirical_cdf[i] == 1){
            break
        }
    }
    res <- empirical_cdf[empirical_cdf != 0]
    cumulative_runs <- checkpoints[1:length(res)]
    builtin_cdf <- pchisq(cumulative_runs, deg_of_freedom)
    p1 <- plot(cumulative_runs, res, type = "l", col = "red")
    lines(cumulative_runs, builtin_cdf, col = "green")
    return(p1)
}
print(cdf_empirical_chi(3, 300))