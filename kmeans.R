######################################
######### K-MEANS CLUSTERING #########
######################################

##
## loading libraries
##

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

##
## kmeans
##

set.seed(2024)
km.out <- kmeans(data, centers = 3, nstart = 5)
km.out

fviz_cluster(km.out, data = data)

#
# we need to classify into three clusters for kmeans to tell us that 2 and 7 
# have a lot of variation :(
#
