  pastries <- list(
    Croissants = list(x = 0:3, p = c(0.1, 0.3, 0.4, 0.2)),
    Muffins = list(x = 0:3, p = c(0.2, 0.4, 0.3, 0.1)),
    Donuts = list(x = 0:3, p = c(0.3, 0.4, 0.2, 0.1))
  )
  
  calculate_stats <- function(x, p) {
    expected <- sum(x * p)
    variance <- sum((x - expected)^2 * p)
    std_dev <- sqrt(variance)
    return(list(Expected = expected, Variance = variance, Std_Dev = std_dev))
  }
  
  for (name in names(pastries)) {
    x <- pastries[[name]]$x
    p <- pastries[[name]]$p
    stats <- calculate_stats(x, p)
    cat("\n", name, ":\n")
    cat("  Expected Value:", stats$Expected, "\n")
    cat("  Variance:", stats$Variance, "\n")
    cat("  Standard Deviation:", stats$Std_Dev, "\n")
  }
  
  par(mfrow = c(1, 3))
  for (name in names(pastries)) {
    x <- pastries[[name]]$x
    p <- pastries[[name]]$p
    barplot(p,
            names.arg = x,
            main = paste(name, "Distribution"),
            xlab = "Number Sold",
            ylab = "Probability",
            col = "red",
            border = "black")
  }
  
  