# Load Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(caTools)  # For sample.split

# Load and Clean Data
data <- read.csv("C:/Users/ravik/OneDrive/Desktop/heart.csv")
data <- na.omit(data)  # Remove missing rows
data$target <- ifelse(data$target == 1, "high-risk", "low-risk")  # Update target variable
data$target <- factor(data$target)  # Convert to factor

# Convert Categorical Columns to Factors
cols_to_factor <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal")
data[cols_to_factor] <- lapply(data[cols_to_factor], factor)

# Split Data (70% Train, 30% Test)
set.seed(123)
split <- sample.split(data$target, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Train Logistic Regression Model
model <- glm(target ~ ., data = train, family = "binomial")

# Predict on Test Data
predictions <- predict(model, test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "high-risk", "low-risk")
predicted_classes <- factor(predicted_classes, levels = c("high-risk", "low-risk"))

# Evaluate Model
confusion <- confusionMatrix(predicted_classes, test$target)
print(confusion)

# Plot Results
ggplot(data.frame(Actual = test$target, Predicted = predicted_classes), aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  labs(title = "Actual vs Predicted Categories", x = "Actual", y = "Count") +
  theme_minimal()

# High-Risk Patients by Gender
ggplot(data %>% filter(target == "high-risk"), aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "High-Risk Patients by Gender", x = "Gender", y = "Count") +
  scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) +
  theme_minimal()
