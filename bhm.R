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

library(RColorBrewer)
cols <- brewer.pal(11, "RdYlGn")
cols
pal <- colorRampPalette(cols)

par(mfrow = c(3,4))
image(a1, col = pal(80), main = "NVDI for 2016")
image(a2, col = pal(80), main = "NVDI for 2017")
image(a3, col = pal(80), main = "NVDI for 2018")
image(a4, col = pal(80), main = "NVDI for 2019")
image(a5, col = pal(80), main = "NVDI for 2020")
image(a6, col = pal(80), main = "NVDI for 2021")
image(a7, col = pal(80), main = "NVDI for 2022")

r <- raster("USGS_OPR_KS_Statewide_2018_A18_14S_PH_2530.tif")
image(r, col = pal(80), main = "Altitude")

par(mfrow = c(2,2))
plot(r2.arch, col = pal(80), main = "Algorithm classification when there is variance",
     xlab = "", ylab = "")
plot(r1.arch, col = pal(80), main = "Algorithm classification when variance is absent",
     xlab = "", ylab = "")

plot(ahatr1, col = pal(80), main = "Bayesian Hierarchical Model classification when there is variance",
     xlab = "", ylab = "")
plot(ahatr2, col = pal(80), main = "Bayesian Hierarchical Model classification when variance is absent",
     xlab = "", ylab = "")

par(mfrow = c(1,1))

levelplot(ahatr2)

matplot(bhm.out[,],type="l")
matplot(bhm.out[-c(1:burnin),],type="l")



par(mfrow = c(2,2))
plot(ahatr1)
plot(ahatr2)
plot(r1.arch)
plot(r2.arch)

colnames(bhm.out)[16014:16034]

h <- post.means[16001:16014]
h <- matrix(h, nrow = 7)
w <- post.means[16015:16028]
betas <- post.means[16029:16030]
mus <- post.means[16031:16032]






