




library(ggplot2)
library(dplyr)
library(rpart)
library(caret)

set.seed(123)



column_names <- c("ID", "CHOL", "SGLU", "HDL", "Y", "LOCATION", "AGE", "GENDER", "HHT", "WHT", "FRAME", "SBP", "DSP", "W", "H")
data <- read.table("Dataset5.txt", header = FALSE, sep = "\t", col.names = column_names)
setIndex <- createDataPartition(data$Y, p = 0.7, list = FALSE)
train_data <- data[setIndex, ]
test_data <- data[-setIndex, ]


model <- glm(Y ~., data = train_data, family = "Gamma")


predictions <- as.matrix(predict(model, newdata = test_data))

summary(model)


mse <- mean((test_data$Y - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data$Y - predictions))

cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")




