heights <- c(150, 160, 170, 180, 190)
weights <- c(55, 60, 70, 80, 85)
model <- lm(weights ~ heights)
summary(model)
coefficients <- coef(model)
coefficients
Weights=0.8*Height+−66
cat("The equation is: Weight =", round(coefficients[2], 2), "* Height +",
    round(coefficients[1], 2), "\n")
new_heights <- data.frame(heights = c(155, 165, 175))
predicted_weights <- predict(model, new_heights)
predicted_weights

