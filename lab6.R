library(caret)
library(caTools)
library(ggplot2)
house_data <- read.csv("C:/Users/ravik/OneDrive/Desktop/house_prices.csv")

house_data <- na.omit(house_data)

set.seed(123)
split <- sample.split(house_data$price, SplitRatio = 0.8)
train_data <- subset(house_data, split == TRUE)
test_data <- subset(house_data, split == FALSE)

model <- lm(price ~ ., data = train_data)

predictions <- predict(model, newdata = test_data)

mse <- mean((predictions - test_data$price)^2)
mae <- mean(abs(predictions - test_data$price))
r_squared <- summary(model)$r.squared

cat("Mean Squared Error:", mse, "\n")
cat("Mean Absolute Error:", mae, "\n")
cat("R-squared:", r_squared, "\n")

ggplot(data = test_data, aes(x = price, y = predictions)) +
  geom_point(color = 'green') +
  geom_abline(color = 'black') +
  theme_minimal()
