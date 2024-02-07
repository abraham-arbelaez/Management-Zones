#######################################################
######### BAYESIAN HIERARCHICAL MODEL WITH AA #########
#######################################################

##
## loading packages
##

library(nimble)


## manipulating r
r <- as.matrix(r)
r <- as.vector(r)

dim(data)

N=nrow(data)
## number of archetypes
K=2
## dim of data
M=ncol(data)

bhm.data=list(Y=data,Z=data, X = r)
bhm.const=list(N=N,K=K,ones.K=rep(1,K),ones.N=rep(1,N))
bhm.inits=list(s2=1,tau2=1,mu=rep(0,K),beta=rep(0,K),
               alpha=matrix(1/K,nrow=N,ncol=K),
               H=matrix(1/K,nrow=N,ncol=K),
               A=matrix(apply(data,2,mean),nrow=M,ncol=K), ## each column 
               ##is an archetype
               W=matrix(1/N,nrow=N,ncol=K))

## model
aa.code <- nimbleCode({
  ## priors for variance params
  s2 ~ dexp(10)
  tau2 ~ dexp(10)
  
  ## priors for regression params 
  for(k in 1:K){
    mu[k] ~ dnorm(0,1)
    beta[k] ~ dnorm(0,.1)
  }
  
  ## priors for archetypes and weights
  for(i in 1:N){
    H[i,1:K] ~ ddirch(alpha[i,1:K])
    for(k in 1:K){
      alpha[i,k] <- exp(beta[k]*X[i])
    }
  }
  for(k in 1:K){
    W[1:N,k] ~ ddirch(ones.N[1:N])
  }
  ## archetype model
  for(k in 1:K){
    for(m in 1:M){
      A[m,k] ~ dnorm(inprod(Z[1:N,m],W[1:N,k]) , sd=sqrt(tau2))
    }
  }
  ## data model
  for(i in 1:N){
    for(m in 1:M){
      Y[i,m] ~ dnorm(inprod(A[m,1:K],H[i,1:K]), sd=sqrt(s2))
    }
  }
})

## run model
## compile model and run MCMC
bhm.out <- nimbleMCMC(aa.code,
                      constants=bhm.const,
                      data=bhm.data,
                      inits=bhm.inits,
                      niter=2000,
                      thin=2,
                      monitors=c("A","H","W","s2","tau2", "beta"))

# write.csv(bhm.out, "bhm.out.csv")

# bhm.out <- read.csv("bhm.out.csv")

matplot(bhm.out[,],type="l")
burnin <- 400
post.means=apply(bhm.out[-c(1:burnin),],2,mean)

A.hat1 <- matrix(post.means[1:8000], nrow=80) 
ahatr1 <- raster(nrows = 80,
                 ncols = 100)
ahatr1[] <- A.hat1
plot(ahatr1)

A.hat2 <- matrix(post.means[8001:16000], nrow=80) 
ahatr2 <- raster(nrows = 80,
                 ncols = 100)
ahatr2[] <- A.hat2
plot(ahatr2)


##
## Spatial component
##

plot(A.hat1)

ah1 <- as.vector(A.hat1)
par(mfrow = c(1,1))
hist(ah1, breaks = 500)

set.seed(2024)
aaaa=stepArchetypes(A.hat1,k=1:8,nrep=3,verbose=TRUE)
par(mfrow = c(1,1))
screeplot(aaaa) # 5 zones

# selecting number 2
bests=bestModel(aaaa[[3]])
As=bests$archetypes

xyplot(bests, A.hat1)



A.hat1t <- t(A.hat1)

ah1 <- as.vector(A.hat1)
par(mfrow = c(1,1))
hist(ah1, breaks = 500)

set.seed(2024)
aaaa=stepArchetypes(A.hat1t,k=1:8,nrep=3,verbose=TRUE)
par(mfrow = c(1,1))
screeplot(aaaa) # 5 zones

# selecting number 2
bests=bestModel(aaaa[[5]])
As=bests$archetypes


##
## low variance year
##

As1 <- As[1,]
As1 <- as.vector(As1)
As11 <- array(As1, dim = c(80,100))
As1m <- matrix(As1, ncol = 100)
As1m

rs1.arch <- raster(nrows = 80, 
                  ncols = 100)
rs1.arch[] <- As1m
plot(rs1.arch)

# 2
As2 <- As[2,]
As2 <- as.vector(As2)
As22 <- array(As2, dim = c(80,100))
As2m <- matrix(As2, ncol = 100)
As2m

rs2.arch <- raster(nrows = 80, 
                   ncols = 100)
rs2.arch[] <- As2m
plot(rs2.arch)

# 3
As3 <- As[3,]
As3 <- as.vector(As3)
As33 <- array(As3, dim = c(80,100))
As3m <- matrix(As3, ncol = 100)
As3m

rs3.arch <- raster(nrows = 80, 
                   ncols = 100)
rs3.arch[] <- As3m
plot(rs3.arch)

# 4
As4 <- As[4,]
As4 <- as.vector(As4)
As44 <- array(As4, dim = c(80,100))
As4m <- matrix(As4, ncol = 100)
As4m

rs4.arch <- raster(nrows = 80, 
                   ncols = 100)
rs4.arch[] <- As4m
plot(rs4.arch)

# 5
As5 <- As[5,]
As5 <- as.vector(As5)
As55 <- array(As5, dim = c(80,100))
As5m <- matrix(As5, ncol = 100)
As5m

rs5.arch <- raster(nrows = 80, 
                   ncols = 100)
rs5.arch[] <- As5m
plot(rs5.arch)

par(mfrow = c(2,3))











