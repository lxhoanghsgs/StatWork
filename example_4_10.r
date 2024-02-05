# My task: modify the method in Example 4.10 to predict the temperature in April 2, which is also Prob. no. 6 (page 269).
temp_in_march <- read.csv(r"(C:\StatWork\advancedstatistics-master\RcodeData\foold.csv)")
mu <- vector(mode = "integer", length = 32)
mu <- replace(mu, mu == 0, 33)
# mu = mean vector.
sigma <- 3
rho <- 0.8
cov_entries <- function(i, j){
    d <- abs(i - j)
    mult <- sigma^2 / (1 - rho^2) 
    res <- rho^d * mult
    return(res)
}
rows <- c(1:31, 33)
cols <- c(1:31, 33)

cov_matrix <- outer(rows, cols, FUN = cov_entries)
# plot(temp_in_march$Day, temp_in_march$Temperature, type = "b")