mu <- 799            
sigma <- 60          
n <- 40               
x_bar <- 760     

z <- (x_bar - mu) / (sigma / sqrt(n))

p_value <- 2 * (pnorm(z))

cat("Z-value:", z, "\n")
cat("P-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. The claim made by the provider is likely false.\n")
} else {
  cat("Conclusion: Do not reject the null hypothesis. The claim may be true.\n")
}
