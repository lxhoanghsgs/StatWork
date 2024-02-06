# Use simulations to verify how robust the result from Theorem 4.37 is to the normal distribution assumption.
# Robustness: changing some small amount changes the scenario completely.
get_regularized_squared_sum <- function(dataset){
    mean <- sum(dataset) / length(dataset)
    res <- 0
    for(i in dataset){
        res <- res + (i - mean)^2
    }
    return(res)
}
cumulative_runs <- seq(0, 10, 0.05)
get_F_cdf_from_chi_square_dist <- function(sample_size = 200, df_num = 4, df_den = 4, m = 3, n = 3){
    sample_by_chi <- vector(mode = "integer", length = sample_size)
    sample_chi_F_cdf <- vector(mode = "integer", length = length(cumulative_runs))
    for(i in 1:sample_size){
        chi_num <- rchisq(m, df = df_num)
        chi_den <- rchisq(n, df = df_den)
        chi_num <- matrix(chi_num, nrow = 1)
        chi_den <- matrix(chi_den, nrow = 1)
        num <- apply(chi_num, MARGIN = 1, FUN = get_regularized_squared_sum)
        den <- apply(chi_den, MARGIN = 1, FUN = get_regularized_squared_sum)
        num <- num / ((m - 1) * sqrt(2 * m))
        den <- den / ((n - 1) * sqrt(2 * n))
        sample_by_chi[i] <- num / den
    }
    for (k in 1:length(sample_chi_F_cdf)){
        sample_chi_F_cdf[k] <- length(sample_by_chi[sample_by_chi <= cumulative_runs[k]]) / sample_size
    }
    return(sample_chi_F_cdf)
}
# Now chi(m) ~ normal(m, 2m).
sample_size <- 2000
df_num <- 5
df_den <- 5
m <- 4
n <- 8
# Even if sample_size is big, when df_num and df_den are small, things are bad.
df_num <- 30
# Worse if one df is too big and the other df is small.
df_den <- 35
# Now better.
d_theoretical <- pf(cumulative_runs, df1 = m - 1, df2 = n - 1)
d_empirical <- get_F_cdf_from_chi_square_dist(sample_size, df_num, df_den, m, n)
plot(cumulative_runs, d_theoretical, type = "l", col = "red")
lines(cumulative_runs, d_empirical, col = "blue")

