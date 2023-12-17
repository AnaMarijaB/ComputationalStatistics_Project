#################################################################
# MCMC
#################################################################

library(dplyr)
library(R2jags)

n_iter <- 20000
n_burnout <- 10000


data_mcmc<- read.csv("Data/data_reduce.csv", header = TRUE)
data_mcmc$beta0 <- 1
p <- ncol(data_mcmc)-1

model_data <- list(
  Y = data_mcmc$GHB,
  x = as.matrix(data_mcmc[, -which(names(data_mcmc) == "GHB")]),
  n = nrow(data_mcmc),
  p = p,
  I = diag(p)
)

model_params <- c("beta","phi","phiinv")
model_Inits <- list(beta=numeric(p)+1, phiinv = 1)
model_jags <- jags(data=model_data, 
                   inits=list(model_Inits), 
                   parameters.to.save=model_params, 
                   "model_txt_files/model_j.txt", 
                   n.chains=1, 
                   n.iter=n_iter, 
                   n.burnin=n_burnout, 
                   DIC=TRUE)
print(model_jags)
chain1 <- as.mcmc(model_jags)

par(mfrow=c(2,1))
traceplot(chain1,col="blue",sub="chain 1")
HPDinterval(chain1,prob=0.95)



#################################################################
# Simple model (alpha=G(0.001,0.001), beta=G(0.001,0.001))
#################################################################

mS1 <- list(
  Y = data_mcmc$GHB,
  n = nrow(data_mcmc)
)

mS1_params <- c("alpha","beta")
mS1_Inits <- list(alpha = 1, beta = 1)
mS1 <- jags(data=mS1, 
         inits=list(mS1_Inits), 
         parameters.to.save=mS1_params, 
         "model_txt_files/model_simple1.txt", # maybe need a[i] <- alpha,...
         n.chains=1, 
         n.iter=n_iter, 
         n.burnin=n_burnout, 
         DIC=TRUE)
print(mS1)
ch_mS1 <- as.mcmc(mS1)

par(mfrow=c(2,1))
traceplot(ch_mS1,col="blue",sub="ch_mS")
HPDinterval(ch_mS1,prob=0.95)







#################################################################
# Simple model (alpha=G(2,1), beta=G(2,1))
#################################################################

mS2 <- list(
  Y = data_mcmc$GHB,
  n = nrow(data_mcmc)
)

mS2_params <- c("alpha","beta")
mS2_Inits <- list(alpha = 1, beta = 1)
mS2 <- jags(data=mS2, 
           inits=list(mS2_Inits), 
           parameters.to.save=mS2_params, 
           "model_txt_files/model_simple2.txt", 
           n.chains=1, 
           n.iter=n_iter, 
           n.burnin=n_burnout, 
           DIC=TRUE)
print(mS2)
ch_mS2 <- as.mcmc(mS2)

par(mfrow=c(2,1))
traceplot(ch_mS2,col="blue",sub="ch_mS")
HPDinterval(ch_mS2,prob=0.95)



#################################################################
# Medium model (alpha=G(0.001,0.001) , 1/beta=G(0.001,0.001))
#################################################################

mM1 <- list(
  Y = data_mcmc$GHB,
  n = nrow(data_mcmc)
)

mM1_params <- c("alpha","beta")
mM1_Inits <- list(alpha = 1, beta = 1)
mM1 <- jags(data=mM1, 
            inits=list(mM1_Inits), 
            parameters.to.save=mM1_params, 
            "model_txt_files/model_medium1.txt", 
            n.chains=1, 
            n.iter=n_iter, 
            n.burnin=n_burnout, 
            DIC=TRUE)
print(mM1)
ch_mM1 <- as.mcmc(mM1)

par(mfrow=c(2,1))
traceplot(ch_mM1,col="blue",sub="ch_mM1")
HPDinterval(ch_mM1,prob=0.95)






#################################################################
# Medium model (alpha=G(2,1), beta=G(2,1))
#################################################################

mM2 <- list(
  Y = data_mcmc$GHB,
  n = nrow(data_mcmc)
)

mM2_params <- c("alpha","beta")
mM2_Inits <- list(alpha = 1, beta = 1)
mM2 <- jags(data=mM2, 
            inits=list(mM2_Inits), 
            parameters.to.save=mM2_params, 
            "model_txt_files/model_medium2.txt", 
            n.chains=1, 
            n.iter=n_iter, 
            n.burnin=n_burnout, 
            DIC=TRUE)
print(mM2)
ch_mM2 <- as.mcmc(mM2)

par(mfrow=c(2,1))
traceplot(ch_mM2,col="blue",sub="ch_mM1")
HPDinterval(ch_mM2,prob=0.95)



#################################################################
# Large model (alpha= bX, 1/beta=1)
#################################################################

mL2 <- list(
  Y = data_mcmc$GHB,
  x = as.matrix(data_mcmc[, -which(names(data_mcmc) == "GHB")]),
  n = nrow(data_mcmc),
  p = p,
  I = diag(p)
)

mL2_params <- c("alpha","beta")
mL2_Inits <- list(alpha = 1, beta=numeric(p)+1)
mL2 <- jags(data=mL2, 
            inits=list(mL2_Inits), 
            parameters.to.save=mL2_params, 
            "model_txt_files/model_large1.txt", 
            n.chains=1, 
            n.iter=n_iter, 
            n.burnin=n_burnout, 
            DIC=TRUE)
print(mL2)
ch_mL2 <- as.mcmc(mL2)

par(mfrow=c(2,1))
traceplot(ch_mL2,col="blue",sub="ch_mL1")
HPDinterval(ch_mL2,prob=0.95)
