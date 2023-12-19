library('R2OpenBUGS')

data_mcmc<- read.csv("Data/data_encoded.csv", header = TRUE)
data_mcmc <- data_mcmc %>% select(-'X', -'H', -'SBP')

model1.data <- list(
  Y = data_mcmc$GHB,
  x = as.matrix(data_mcmc[, -which(names(data_mcmc) == "GHB")]),
  n = nrow(data_mcmc)
)

model1.params <- c("beta","phi","phiinv")

model1.Inits <- list(beta=numeric(12), phiinv = 1)

model1.fit <- bugs(data=model1.data, inits=list(model1.Inits), parameters.to.save=model1.params, 
                          "model1.txt", n.chains=1, n.iter=100, n.burnin=10, debug=FALSE, save.history=FALSE, DIC=TRUE)
