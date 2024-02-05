# Investigate the normal approx (CLT = central limit theorem): chi^2(n) ~ N(n, 2n).
library(ggplot2)
number_of_observations <- 2000
deg_of_freedom <- 100
chi_dist <- rchisq(n = number_of_observations, df = deg_of_freedom)
norm_dist <- rnorm(number_of_observations, mean = deg_of_freedom, sd = sqrt(2 * deg_of_freedom))
# Notice that this is a little bit different: sd(X) = sqrt(DX)
c_data <- data.frame(chi_dist)
n_data <- data.frame(norm_dist)
c_data$dname <- "Chi-squared"
n_data$dname <- "Normal"
colnames(c_data) <- c("values", "dname")
colnames(n_data) <- c("values", "dname")
comparing_data <- rbind(c_data, n_data)
ggplot(comparing_data, aes(values, fill = dname)) + geom_density(alpha = 0.2)