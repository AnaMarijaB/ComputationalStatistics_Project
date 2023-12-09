#################################################################
# GAMMA - GLM
#################################################################




library(ggplot2)
library(dplyr)
library(rpart)
library(caret)

set.seed(123)




data <- read.csv("Data/ful_data.csv")

colnames(data)[colnames(data) == "GHB"] <- "Y"
setIndex <- createDataPartition(data$Y, p = 0.7, list = FALSE)
train_data <- data[setIndex, ]
test_data <- data[-setIndex, ]


model <- glm(Y ~., data = train_data, family = "Gamma")
summary(model)

confint(model)


predictions <- as.matrix(predict(model, newdata = test_data))
pred <- predict(model, newdata = test_data)
residuals <- residuals(model, type = 'deviance')

plot(predictions, residuals)


mse <- mean((test_data$Y - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data$Y - predictions))

cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")




