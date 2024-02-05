# Compare the t-distribution and the standard normal distribution.
cumulative_runs <- seq(-6, 6, 0.05)
t_dist3 <- pt(cumulative_runs, df = 3)
t_dist7 <- pt(cumulative_runs, df = 7)
ndist <- pnorm(cumulative_runs, mean = 0, sd = 1)
p1 <- plot(cumulative_runs, t_dist3, type = "l", col = "red")
lines(cumulative_runs, t_dist7, type = "l", col = "green")
lines(cumulative_runs, ndist, type = "l", col = "blue")
legend(-4, 0.8, legend = "Red: n=3\nGreen: n=7\nBlue: Normal")
