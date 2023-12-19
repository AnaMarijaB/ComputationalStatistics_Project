#################################################################
# MCMC
#################################################################

library(dplyr)
library(R2jags)

n_iter <- 20000
n_burnout <- 10000

data_mcmc <- read.csv("Data/data_final.csv", header = TRUE)
data_mcmc <- data_mcmc %>% select(-{'X'})
data_mcmc$beta0 <- 1
data_mcmc <- data_mcmc %>%
  select(beta0, everything())
p <- ncol(data_mcmc)-1

model_data <- list(
  Y = data_mcmc$GHB,
  x = as.matrix(data_mcmc[, -which(names(data_mcmc) == "GHB")]),
  n = nrow(data_mcmc),
  p = p,
  I = diag(p)
)

# MODEL 1

model_params1 <- c("beta","phi","phiinv")
model_Inits1 <- list(beta = rep(0, p), phiinv = 1)

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

par(mfrow=c(8,1))
traceplot(chain1,col="blue",sub="chain 1")
HPDinterval(chain1,prob=0.95)

# MODEL 2

model_params2 <- c("beta","phi","phiinv")
model_Inits2 <- list(beta = rep(0, p), phiinv = 1)

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

par(mfrow=c(2,1))
traceplot(chain2,col="blue",sub="chain 1")
HPDinterval(chain2,prob=0.95)











