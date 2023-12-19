#################################################################
# GAMMA - GLM
#################################################################

library(ggplot2)
library(dplyr)
library(rpart)
library(caret)
library(DHARMa)

set.seed(123)

data_glm <- read.csv("Data/data_encoded.csv", header = TRUE)
data_glm <- data_glm %>% select(-{'X'})

colnames(data_glm)[colnames(data_glm) == "GHB"] <- "Y"
setIndex <- createDataPartition(data_glm$Y, p = 0.7, list = FALSE)
train_data <- data_glm[setIndex, ]
test_data <- data_glm[-setIndex, ]

################################################################
# link function = 'inverse'
###############################################################

mod_all <- glm(Y ~ ., data = data_glm, family = Gamma(link='inverse'))
summary(mod_all)

# korelacije W, H, WHT

mod_mw <- glm(Y ~ . - W, data = data_glm, family = Gamma(link='inverse'))
mod_mh <- glm(Y ~ . - H, data = data_glm, family = Gamma(link='inverse'))
mod_mwh <- glm(Y ~ . - W - H, data = data_glm, family = Gamma(link='inverse'))

anova(mod_all, mod_mw, test = 'Chisq') 
anova(mod_all, mod_mh, test = 'Chisq')
anova(mod_all, mod_mwh, test = 'Chisq') # vzamemo tega = -w-h

# korelacije SBP, DSP

mod_msbp <- glm(Y ~ . - W - H - SBP, data = data_glm, family = Gamma(link='inverse'))
mod_mdsp <- glm(Y ~ . - W - H - DSP, data = data_glm, family = Gamma(link='inverse'))

anova(mod_mwh, mod_msbp, test = 'Chisq') #vzamemo tega -w-h-sbp
anova(mod_mwh, mod_mdsp, test = 'Chisq')

res1 <- residuals(mod_msbp, type = 'deviance')
pred1 <- predict(mod_msbp, type='response')
plot(pred1, res1)

check_gamma_model <- simulateResiduals(fittedModel = mod_msbp)
plot(check_gamma_model)


################################################################
# link function = 'log'
###############################################################

mod_all_log <- glm(Y ~ ., data = data_glm, family = Gamma(link='log'))
summary(mod_all_log)

# korelacije W, H, WHT

mod_mw_log <- glm(Y ~ . - W, data = data_glm, family = Gamma(link='log'))
mod_mh_log <- glm(Y ~ . - H, data = data_glm, family = Gamma(link='log'))
mod_mwh_log <- glm(Y ~ . - W - H, data = data_glm, family = Gamma(link='log'))

anova(mod_all_log, mod_mw_log, test = 'Chisq') 
anova(mod_all_log, mod_mh_log, test = 'Chisq') #vzamemo tega -h
anova(mod_all_log, mod_mwh_log, test = 'Chisq') 

# korelacije SBP, DSP

mod_msbp_log <- glm(Y ~ . - H - SBP, data = data_glm, family = Gamma(link='log'))
mod_mdsp_log <- glm(Y ~ . - H - DSP, data = data_glm, family = Gamma(link='log'))

anova(mod_mw_log, mod_msbp_log, test = 'Chisq') #vzamemo tega -h- sbp
anova(mod_mw_log, mod_mdsp_log, test = 'Chisq')

res2 <- residuals(mod_msbp_log, type = 'deviance')
pred2 <- predict(mod_msbp_log, type='response')
plot(pred2, res2)

check_gamma_model2 <- simulateResiduals(fittedModel = mod_msbp_log)
plotSimulatedResiduals(check_gamma_model2)

# odločimo se vzeti regresijo brez h in sbp 





