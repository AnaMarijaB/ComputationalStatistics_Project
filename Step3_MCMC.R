#################################################################
# MCMC
#################################################################

library(dplyr)
library(R2jags)

n_iter <- 50000
n_burnout <- 5000

data_mcmc <- read.csv("Data/data_final.csv", header = TRUE)
data_mcmc <- data_mcmc %>% select(-{'X'})
data_mcmc$delta0 <- 1
data_mcmc <- data_mcmc %>%
 select(delta0, everything())
p <- ncol(data_mcmc)-1

model_data <- list(
  Y = data_mcmc$GHB,
  x = as.matrix(data_mcmc[, -which(names(data_mcmc) == "GHB")]),
  n = nrow(data_mcmc),
  p = p,
  I = diag(p)
)

# MODEL 1

model_params1 <- c("delta","alpha","alphainv")
model_Inits1 <- list(delta = rep(0, p), alphainv = 1)

model_jags1 <- jags(data=model_data, 
                   inits=list(model_Inits1), 
                   parameters.to.save=model_params1, 
                   "model_txt_files/model1.txt", 
                   n.chains=1, 
                   n.iter=n_iter, 
                   n.burnin=n_burnout, 
                   DIC=TRUE)

print(model_jags1)
chain1 <- as.mcmc(model_jags1)

important <- chain1[, c('delta[1]', 'delta[2]','delta[3]', 'delta[4]','delta[5]', 'alpha')]
par(mfrow=c(6,1))
traceplot(important,col="blue",sub="")

geweke.diag(chain1, frac1 = 0.1, frac2 = 0.5)
heidel.diag(chain1)
HPDinterval(chain1,prob=0.95)

# MODEL 2

model_params2 <- c("delta","alpha","alphainv")
model_Inits2 <- list(delta = rep(0, p), alphainv = 1)

model_jags2 <- jags(data=model_data, 
                   inits=list(model_Inits2), 
                   parameters.to.save=model_params2, 
                   "model_txt_files/model2.txt", 
                   n.chains=1, 
                   n.iter=n_iter, 
                   n.burnin=n_burnout, 
                   DIC=TRUE)

print(model_jags2)
chain2 <- as.mcmc(model_jags2)

important2 <- chain2[, c('delta[1]', 'delta[2]','delta[3]', 'delta[4]','delta[5]', 'alpha')]
par(mfrow=c(6,1))
traceplot(important2,col="blue",sub="")

geweke.diag(chain2, frac1 = 0.1, frac2 = 0.5)
heidel.diag(chain2)
HPDinterval(chain2,prob=0.95)








