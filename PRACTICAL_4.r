miu <- 10000 
sample_mean <- 9900
 sigma <- 120 n <- 30
 alpha <- 0.5
 z <- (sample_mean - miu) / (sigma / sqrt(n))
 p_value <- pnorm(z) 
cat("Z statistic:", z, "\n")
 cat("P-value:", p_value, "\n")
 if (p_value> 0.5) 
{ cat("Conclusion: Pass null hypothesis.\n")
} else
 {
 cat("Conclusion: Reject the null hypothesis\n") 
}
