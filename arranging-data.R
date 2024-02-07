#####################################
######### DATA MANIPULATION #########
#####################################

##
## matrix values for all years
##

a1 <- as.matrix(a1)
a2 <- as.matrix(a2)
a3 <- as.matrix(a3)
a4 <- as.matrix(a4)
a5 <- as.matrix(a5)
a6 <- as.matrix(a6)
a7 <- as.matrix(a7)

##
## having them as vectors
##

v1 <- as.vector(a1)
v2 <- as.vector(a2)
v3 <- as.vector(a3)
v4 <- as.vector(a4)
v5 <- as.vector(a5)
v6 <- as.vector(a6)
v7 <- as.vector(a7)

##
## creating big dataframe (7 years in rows and 8000 cells in columns)
##

data <- rbind(v1, v2, v3, v4, v5, v6, v7)

##
## fun statistics
##

# sums
sums <- as.vector(mode = "numeric", 8000)
for(i in 1:8000){
  sums[i] <- sum(data[,i])
}

par(mfrow = c(1,1))
plot(sums)

# means
means <- as.vector(mode = "numeric", 8000)
for(i in 1:8000){
  means[i] <- mean(data[,i])
}

plot(means)
hist(means, breaks = 200)

# sd
sds <- as.vector(mode = "numeric", 8000)
for(i in 1:8000){
  sds[i] <- sd(data[,i])
}

plot(sds)
hist(sds, breaks = 100)
