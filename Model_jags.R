install.packages("R2OpenBUGS")
library(R2OpenBUGS)
library(dplyr)

# for installing jags
library(devtools)
devtools::install_url("http://sourceforge.net/projects/mcmc-jags/files/rjags/4/rjags_4-4.tar.gz",
                      args="--configure-args='--with-jags-include=/Users/casallas/homebrew/opt/jags/include/JAGS
--with-jags-lib=/Users/casallas/homebrew/opt/jags/lib'
"
)
install.packages("R2jags")
library(R2jags)





data_mcmc<- read.csv("Data/data_encoded.csv", header = TRUE)
data_mcmc <- data_mcmc %>% select(-'X', -'H', -'SBP')

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




".RNG.seed" <- 314159
model_jags <- jags(data=model_data, 
                   inits=list(model_Inits), 
                   parameters.to.save=model_params, 
                   "model_j.txt", 
                   n.chains=1, 
                   n.iter=1000, 
                   n.burnin=100, 
                   DIC=TRUE)
model_jags



chain2 <- as.mcmc(model_jags)
