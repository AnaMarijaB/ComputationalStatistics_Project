library('R2OpenBUGS')
library('coda')

Example <- read.table("examples/Example_Line_Data1.txt",header=T)
dim(Example)
head(round(Example,3))

Y <- Example$Y
x <- Example$x
n <- nrow(Example)

Example.data1 <-  list("Y","x","n")
Example.params1 <- c("beta","tau","sigma")

Example.Inits1 <- list(tau=1,beta=c(0,0))

Example_BUGS.fit1 <- bugs(data=Example.data1, inits=list(Example.Inits1), parameters.to.save=Example.params1, 
                          "Example_Line_Bugs1.txt", n.chains=1, n.iter=20000, n.burnin=10000, debug=FALSE, save.history=FALSE, DIC=TRUE)
#n.burnin = kolko samplov parametrov nardimo

Example_BUGS.fit1$summary

names(Example_BUGS.fit1)

Example_BUGS.fit1$DIC
Example_BUGS.fit1$pD

Example_BUGS.fit1$sims.matrix

# process convergence study

chain1 <- as.mcmc(Example_BUGS.fit1$sims.matrix[,c(1:2,4)])
head(chain1,9)

# Trace plot
par(mfrow=c(3,1))
traceplot(chain1,col="blue",sub="chain 1")

dev.off()

# Convergence diagnosis methods
geweke.diag(chain1,frac1=0.1,frac2=0.5)
heidel.diag(chain1)
raftery.diag(chain1)

# 95% HPD credible interval
HPDinterval(chain1,prob=0.95)








