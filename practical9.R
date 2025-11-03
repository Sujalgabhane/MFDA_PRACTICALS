data <- data.frame(
  Price = c(250000, 320000, 180000, 540000, 450000, 390000, 210000, 610000, 300000, 470000),
  Area = c(1500, 1800, 1200, 2500, 2300, 2000, 1300, 3000, 1600, 2200),
  Bedrooms = c(3, 4, 2, 5, 4, 3, 2, 5, 3, 4),
  Distance = c(5, 7, 15, 3, 4, 6, 14, 2, 8, 5)
)

model <- lm(Price ~ Area + Bedrooms + Distance, data = data)
summary(model)

new <- data.frame(Area = c(1800, 2500), Bedrooms = c(3, 4), Distance = c(6, 3))
predict(model, new)
