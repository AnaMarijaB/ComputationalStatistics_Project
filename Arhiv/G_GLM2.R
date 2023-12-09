#################################################################
# GAMMA - GLM
#################################################################

library(ggplot2)
library(dplyr)
library(rpart)
library(caret)

set.seed(123)

data_glm <- read.csv("Data/data_encoded.csv", header = TRUE)
data_glm <- data_glm %>% select(-{'X'})

colnames(data_glm)[colnames(data_glm) == "GHB"] <- "Y"
setIndex <- createDataPartition(data_glm$Y, p = 0.7, list = FALSE)
train_data <- data_glm[setIndex, ]
test_data <- data_glm[-setIndex, ]


# intercept-only model. Y = a + bX
model <- glm(Y ~ 1, data = data_glm, family = Gamma(link='log'))
summary(model)
#spodnji dve vrednosti sta isti
mean(data$Y)
exp(coef(model))

shape <- 1/(summary(model)$dispersion)
scale <- as.numeric(exp(coef(model)))/shape

hist(data$Y, freq=FALSE, ylim=c(0, 0.08))
curve(dgamma(x, shape =shape, scale=scale), from = 0, to = 15, col='red', add=TRUE)

###########################################
# function of AGE
###########################################

mod_age <- glm(Y ~ AGE, data = data, family = Gamma(link='log'))
summary(mod_age)

shape_age <- 1/(summary(mod_age)$dispersion)
scale_age <- as.numeric(exp(coef(mod_age)))/shape_age

hist(data$Y, freq=FALSE, ylim=c(0, 0.08))
curve(dgamma(x, shape =shape_age, scale=scale_age), from = 0, to = 15, col='red', add=TRUE)

predict(mod2, test_data, type='response')

sims <- simulate(mod2, nsim=50)
plot(density(data$Y))
for (i in 1:50) lines(density(sims[[i]]), col='grey80')

library(ggeffects)
plot(ggpredict(mod_age, terms = "AGE"), rawdata = TRUE, 
     labels = scales::dollar) +
  ggplot2::ggtitle("Predicted values of Y: gamma model")


# FUNCTION OF ALL 
mod3 <- glm(Y ~ ., data = data_glm, family = Gamma(link='log'))

AIC(model, mod2, mod3)

summary(mod3)

shape3 <- 1/(summary(mod3)$dispersion)
scale3 <- as.numeric(exp(coef(mod3)))/shape3

hist(data$Y, freq=FALSE, ylim=c(0, 0.08))
curve(dgamma(x, shape =shape2, scale=scale2), from = 0, to = 15, col='red', add=TRUE)

predict(mod3, test_data, type='response')

sims <- simulate(mod3, nsim=50)
plot(density(data$Y))
for (i in 1:50) lines(density(sims[[i]]), col='grey80')

# FUNCTION OF ALL - W
mod_mw <- glm(Y ~ . - W, data = data_glm, family = Gamma(link='log'))
summary(mod_mw)
AIC(mod3, mod_mw)

# FUNCTION OF ALL - W - H
mod_mwh <- glm(Y ~ . - W - H, data = data_glm, family = Gamma(link='log'))
summary(mod_mwh)
AIC(mod3, mod_mw, mod_mwh)

# FUNCTION OF ALL - H
mod_mh <- glm(Y ~ . - H, data = data_glm, family = Gamma(link='log'))
summary(mod_mh)
AIC(mod3, mod_mw, mod_mwh, mod_mh)

# Vzamemo model samo brez W

# FUNCTION OF ALL - DSP
mod_mdsp <- glm(Y ~ . - W - DSP, data = data_glm, family = Gamma(link='log'))
summary(mod_mdsp)
AIC(mod3, mod_mw, mod_mdsp)

# FUNCTION OF ALL - SBP
mod_msbp <- glm(Y ~ . - W - SBP, data = data_glm, family = Gamma(link='log'))
summary(mod_msbp)
AIC(mod3, mod_mw, mod_mdsp, mod_msbp)

# Vzamemo model samo brez w in brez SBP






c("ID", "CHOL", "SGLU", "HDL", "GHB", "LOCATION", "AGE", "GENDER", "HHT", "WHT", "FRAME", "SBP", "DSP", "W", "H")








