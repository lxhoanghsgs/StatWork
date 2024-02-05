library(MASS)
library(rgl)
# This generates multivariate normally distributed pseudo-random variables.
sample_data <- matrix(vector(mode = "integer", length = 600), nrow = 200, ncol = 3)
for (i in 1:200){
    sample_iid_standard_normal <- mvrnorm(n = 1, mu = c(0, 0, 0), Sigma = diag(3))
    D <- sample_iid_standard_normal
    X <- D[1] + D[2] - D[3]
    Y <- -D[1] + D[2]
    Z <- D[1] + D[3]
    sample_data[i, 1] <- X
    sample_data[i, 2] <- Y
    sample_data[i, 3] <- Z
}
# Drawing. A little bit silly.
# Conditional independence: P(A|B, C) = P(A|C). A: hypothesis, B, C: observartions.
plot3d(sample_data[, 1], sample_data[, 2], sample_data[, 3], xlab = "X", ylab = "Y", zlab = "Z", type = "s", size = 0.5)

