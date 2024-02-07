######################################
######### ARCHETYPE ANALYSIS #########
######################################

##
## loading packages
##

library(archetypes)
library(rasterVis)

####rasterVis
## archetype analysis
##

set.seed(2024)
aaa=stepArchetypes(data,k=1:8,nrep=3,verbose=TRUE)
par(mfrow = c(1,1))
screeplot(aaa) #archetype 2

# selecting number 2
best=bestModel(aaa[[2]])
A=best$archetypes

##
## low variance year
##

A1 <- A[1,]
A1 <- as.vector(A1)
A11 <- array(A1, dim = c(80,100))
A1m <- matrix(A1, ncol = 100)
A1m

r1.arch <- raster(nrows = 80, 
                  ncols = 100)
r1.arch[] <- A1m
plot(r1.arch)

##
## high variance
##

A2 <- A[2,]
A22 <- array(A2, dim = c(80,100))
A2m <- matrix(A2, ncol = 100)
A2m

r2.arch <- raster(nrows = 80, 
                  ncols = 100)
r2.arch[] <- A2m
plot(r2.arch)

##
## plots for both
##

par(mfrow = c(1,2))
plot(r1.arch)
plot(r2.arch)

par(mfrow = c(1,1))

## 
## cool plots
##

plot1 <- levelplot(r1.arch, contour = TRUE)
plot2 <- levelplot(r2.arch, contour = TRUE)

plot1 
plot2
