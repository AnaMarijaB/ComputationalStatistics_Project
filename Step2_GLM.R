#################################################################
# GAMMA - GLM
#################################################################
library(dplyr)
library(caret)

set.seed(123)

data <- read.csv("Data/data_encoded.csv", header = TRUE)
data <- data %>% select(-{'X'})

i <- createDataPartition(data$GHB, p = 0.8, list = FALSE)
# to check performance of GLM
train <- data[i, ]
test <- data[-i, ]
link_fun <-"log"

#################################################################
# Full data GLM performance
#################################################################
glmFull <- glm(GHB ~ .,data = data, family = Gamma(link = link_fun))
pred <- predict(glmFull, newdata = test, type = "response")

# Decision based on 
mse <- mean((pred - test$GHB)^2)
AIC_value <- AIC(glmFull)
BIC_value <- BIC(glmFull)

#################################################################
# 1. Coefficient Magnitude
#################################################################

coefficients <- coef(glmFull)
coefficients <- coefficients[-1] # exclude the intercept 
top5_magnitude <- names(head(sort(abs(coefficients), decreasing = TRUE), 5))
top5_magnitude <- c(top5_magnitude, "GHB")
train_coef <- train[top5_magnitude]
test_coef <- test[top5_magnitude]

glmCoef <-  glm(GHB ~ .,data = train_coef, family = Gamma(link = link_fun))
predCoef <- predict(glmCoef, newdata = test_coef, type = "response")
mseCoef <- mean((predCoef - test_coef$GHB)^2)
AIC_value_coef <- AIC(glmCoef)
BIC_value_coef <- BIC(glmCoef)

#################################################################
# 2. P-values
#################################################################

p_values <- summary(glmFull)$coefficients[, 4]
p_values <- p_values[-1]
top5_p_values <- names(head(sort(p_values), 5))
top5_p_values <- c(top5_p_values, "GHB")
train_p_values <- train[top5_p_values]
test_p_values <- test[top5_p_values]

glmPvalue <-  glm(GHB ~ .,data = train_p_values, family = Gamma(link = link_fun))
predPvalue <- predict(glmPvalue, newdata = test_p_values, type = "response")
# Results:
msePvalue <- mean((predPvalue - test_p_values$GHB)^2)
AIC_value_Pvalue <- AIC(glmPvalue)
BIC_value_Pvalue <- BIC(glmPvalue)

#################################################################
# 3. Confidence Intervals
#################################################################

conf_intervals <- confint(glmFull)
lower_limits <- conf_intervals[, 1]
upper_limits <- conf_intervals[, 2]
lower_limits <- lower_limits[-1]
upper_limits <- upper_limits[-1]

significant_variables <- names(which(lower_limits * upper_limits > 0))
significant_variables <- c(significant_variables, "GHB")
train_significant <- train[significant_variables]
test_significant <- test[significant_variables]

glmConf <-  glm(GHB ~ .,data = train_significant, family = Gamma(link = link_fun))
predConf <- predict(glmConf, newdata = test_significant, type = "response")
# Results:
mseConf <- mean((predConf - test_significant$GHB)^2)
AIC_value_Conf <- AIC(glmConf)
BIC_value_Conf <- BIC(glmConf)

#################################################################
# 4. Deviance & Chi-squared tests
#################################################################

chi2_test_results <- anova(glmFull, test = "Chi")
deviance_chi_sq <- data.frame(
  Variable = rownames(chi2_test_results),
  Deviance = chi2_test_results[, "Deviance"]
)
top5_deviance <- deviance_chi_sq[-1, ] %>% arrange(Deviance) %>% head(5) 
top5_deviance <- c(top5_deviance$Variable, "GHB")
train_chi2 <- train[top5_deviance]
test_chi2 <- test[top5_deviance]

glm_chi2 <- glm(GHB ~ ., data = train_chi2, family = Gamma(link = link_fun))
pred_chi2 <- predict(glm_chi2, newdata = test_chi2, type = "response")
# Results:
mse_chi2 <- mean((pred_chi2 - test_chi2$GHB)^2)
AIC_chi2 <- AIC(glm_chi2)
BIC_chi2 <- BIC(glm_chi2)

#################################################################
# EDA based
#################################################################

eda_col <- c("CHOL", "SGLU", "AGE", "W", "GHB")
train_eda <- train[eda_col]
test_eda <- test[eda_col]

glm_reduce <- glm(GHB ~ CHOL + SGLU + AGE + W, data = train_eda, family = Gamma(link = link_fun))
pred_eda <- predict(glm_reduce, newdata = test_eda, type = "response")
# Results:
mse_eda <- mean((pred_eda - test_eda$GHB)^2)
AIC_eda <- AIC(glm_reduce)
BIC_eda <- BIC(glm_reduce)

#############################################################################################
#############################################################################################

# Create a data frame to store the results
results_df <- data.frame(
  Importance_Method = c("Full Model", "Coefficient Magnitude", "P-values", "Confidence Intervals", "Deviance/Chi-squared", "EDA"),
  MSE = numeric(6),
  AIC = numeric(6),
  BIC = numeric(6)
)

# Full Model results
results_df[1, "MSE"] <- mse
results_df[1, "AIC"] <- AIC_value
results_df[1, "BIC"] <- BIC_value

# Coefficient Magnitude results
results_df[2, "MSE"] <- mseCoef
results_df[2, "AIC"] <- AIC_value_coef
results_df[2, "BIC"] <- BIC_value_coef

# P-values results
results_df[3, "MSE"] <- msePvalue
results_df[3, "AIC"] <- AIC_value_Pvalue
results_df[3, "BIC"] <- BIC_value_Pvalue

# Confidence Intervals results
results_df[4, "MSE"] <- mseConf
results_df[4, "AIC"] <- AIC_value_Conf
results_df[4, "BIC"] <- BIC_value_Conf

# Deviance/Chi-squared results
results_df[5, "MSE"] <- mse_chi2
results_df[5, "AIC"] <- AIC_chi2
results_df[5, "BIC"] <- BIC_chi2

# EDA
results_df[6, "MSE"] <- mse_eda
results_df[6, "AIC"] <- AIC_eda
results_df[6, "BIC"] <- BIC_eda

# Print the results data frame
print(results_df)

# Define which model you want to use in next steps
data_new <- data[eda_col]
model <- glm(GHB ~ CHOL + SGLU + AGE + W, data = data_new, family = Gamma(link = link_fun))

summary(model)

par(mfrow = c(2, 2))
plot(model, which = 1)  # Residuals vs. Fitted values
plot(model, which = 2)  # Normal Q-Q plot of residuals
plot(model, which = 3)  # Scale-Location plot
plot(model, which = 5)  # Cook's distance plot

cooksd <- cooks.distance(model)
influential_observations <- which(cooksd > 0.05)
influential_rows <- data[influential_observations, ]
print(influential_rows)

#################################
# Distribution check
#################################

gamma_values <- model$fitted.values
df_gamma <- data.frame(x = gamma_values)

# Create a data frame for data$GHB
df_data <- data.frame(x = data_new$GHB)

# Plot histogram with data_new$GHB overlay
gghistplot <- ggplot() +
  geom_density(data = df_gamma, aes(x = x, y = stat(density), fill = "Fitted model distribution"), alpha = 0.7) +
  geom_histogram(data = df_data, aes(x = x, y = stat(density), fill = "Data histogram"), alpha = 0.7, binwidth = 0.5) +
  labs(title = "", x = "GHB", y = "Relative frequency") +
  scale_fill_manual(values = c("Fitted model distribution" = "cadetblue", "Data histogram" = "darkorange"), guide = guide_legend(title = "")) +
  theme(legend.position = "bottom") 



##################################
# Saving final data
##################################

write.csv(data_new, "Data/data_final.csv", row.names = TRUE)


##################################
# Interpretation
##################################

# scale
inter_data <- data_new
Y <- inter_data$GHB
inter_data <- as.data.frame(scale(inter_data))
inter_data$GHB <- Y
summary(inter_data)

model_stand <- glm(GHB ~ CHOL + SGLU + AGE + W, data = inter_data, family = Gamma(link = link_fun))
print(model_stand$coefficients)


# Create 3D scatter plot
library(scatterplot3d)

# true
data_new$color <- ifelse(data_new$GHB > 7, "red", "skyblue")
scatterplot3d(
  data_new$CHOL, data_new$SGLU, data_new$AGE,
  color = data_new$color,
  main = "3D Plot of diagnosis of diabetes (true)",
  pch = 16,
  xlab = "CHOL",
  ylab = "SLGU",
  zlab = "AGE"
)
legend("topleft", legend = c("GHB > 7", "GHB <= 7"), col = c("red", "skyblue"), pch = 16, bty = "n")

# pred
newGHB <- predict(model, newdata = data_new, type = "response")
data_new$GHB <- newGHB
data_new$color1 <- ifelse(data_new$GHB > 7, "red", "skyblue")
# Create 3D scatter plot
scatterplot3d(
  data_new$CHOL, data_new$SGLU, data_new$AGE,
  color = data_new$color1,
  main = "3D Plot of diagnosis of diabetes (prediction)",
  pch = 16,
  xlab = "CHOL",
  ylab = "SLGU",
  zlab = "AGE"
)
legend("topleft", legend = c("GHB > 7", "GHB <= 7"), col = c("red", "skyblue"), pch = 16, bty = "n")

