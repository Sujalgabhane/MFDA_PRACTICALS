mu <- 400         
x_bar <- 250      
n <- 50            
sigma <- 25       
s <- 15          
alpha<-0.05

z_stat <- (x_bar - mu) / (sigma / sqrt(n))


p_value_z <- 2 * (pnorm( (z_stat)))

cat("Z statistic =", z_stat, "\n")
cat("P-value =", p_value_z, "\n")


if (p_value_z < alpha ) {
  cat("Result: Reject the null hypothesis. The claim is not supported.\n\n")
} else {
  cat("Result: Accept the null hypothesis. The claim is supported.\n\n")
}
