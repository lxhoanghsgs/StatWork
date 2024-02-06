# Demonstrate formula (4.31) by simulations.
library(MASS)
library(ggplot2)
sample_size <- 800
m <- 4
n <- 8
mu_m <- vector(mode = "integer", length = m)
mu_n <- vector(mode = "integer", length = n)
cov_matrix_m <- diag(m)
cov_matrix_n <- diag(n)
fisher_check <- rf(sample_size, m - 1, n - 1)
X <- mvrnorm(sample_size, mu_m, cov_matrix_m)
Y <- mvrnorm(sample_size, mu_n, cov_matrix_n)
empirical_F <- vector(mode = "integer", length = sample_size)
for (i in 1:sample_size){
    X_bar <- sum(X[i, ]) / m
    Y_bar <- sum(Y[i, ]) / n
    den <- 0
    num <- 0
    for(j in 1:m){
        num <- num + (X[i, j] - X_bar)^2
    }
    num <- num / (m - 1)
    for(k in 1:n){
        den <- den + (Y[i, k] - Y_bar)^2
    }
    den <- den / (n - 1)
    empirical_F[i] <- num / den
}
theoretical_F <- rf(sample_size, m - 1, n - 1)
empirical_F_data <- data.frame(empirical_F)
theoretical_F_data <- data.frame(theoretical_F)
empirical_F_data$method <- "empirical"
theoretical_F_data$method <- "theoretical"
colnames(empirical_F_data) <- c("values", "method")
colnames(theoretical_F_data) <- c("values", "method")
comparing_data <- rbind(empirical_F_data, theoretical_F_data)
ggplot(comparing_data, aes(values, fill = method)) + geom_density(alpha = 0.2)

