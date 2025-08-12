  mean_height <- 175
  sd_height <- 10
  
  x <- seq(mean_height - 40, mean_height + 40, length=100)
  y <- dnorm(x, mean=mean_height, sd=sd_height)
  
  plot(x, y, type="l", col="blue", lwd=2,
       main="Normal Distribution of Heights",
       xlab="Height (cm)", ylab="Density")
  
  prob_less_165 <- pnorm(165, mean=mean_height, sd=sd_height)
  print(paste("P(height < 165) =", round(prob_less_165, 4)))
  
  prob_between_170_180 <- pnorm(180, mean=mean_height, sd=sd_height) - pnorm(170, mean=mean_height, sd=sd_height)
  print(paste("P(170 < height < 180) =", round(prob_between_170_180, 4)))
  
  height_90th <- qnorm(0.9, mean=mean_height, sd=sd_height)
  print(paste("90th percentile height =", round(height_90th, 2), "cm"))
  
  set.seed(1)
  sample_heights <- rnorm(1000, mean=mean_height, sd=sd_height)
  
  hist(sample_heights, breaks=30, probability=TRUE,
       main="Histogram of Sampled Heights",
       xlab="Height (cm)", col="pink")
  
  lines(x, y, col="gold", lwd=2)
  
  
  
  