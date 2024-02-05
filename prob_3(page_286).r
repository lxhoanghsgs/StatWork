# Demonstrate by simulations: an occurence of t-distribution.
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

empirical_student_cdf <- function(deg_of_freedom, number_of_observations){
    X <- rchisq(number_of_observations, df = deg_of_freedom)
    Y <- rchisq(number_of_observations, df = deg_of_freedom)
    sample_t <- 0.5 * sqrt(deg_of_freedom) * (X - Y) / sqrt(X * Y)
    cumulative_runs <- seq(-6, 6, 0.05)
    empirical_t_cdf <- vector(mode = "integer", length = length(cumulative_runs))
    for(i in 1:length(cumulative_runs)){
        empirical_t_cdf[i] <- count_for_cdf(sample_t, cumulative_runs[i])/number_of_observations
    }    
    p1 <- plot(cumulative_runs, empirical_t_cdf, type = "l", col = "red")
    return(p1)
}
empirical_student_cdf(4, 200)
lines(cumulative_runs, pt(cumulative_runs, 4), col = "green")