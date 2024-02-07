########################
######### DATA #########
########################

##
## loading packages
##

library(raster)
library(spatstat)
library(sf)

##
## setting directory for Kansas 
##

setwd("~/Library/CloudStorage/OneDrive-KansasStateUniversity/Zone Management/Zone Management/data/KS")

##
## reading data
##

field_shape <- st_read("field_KS.shp")
extent <- field_shape

r <- raster("USGS_OPR_KS_Statewide_2018_A18_14S_PH_2530.tif")
a <- raster("USGS_OPR_KS_Statewide_2018_A18_14S_PH_2530_WGS84.tif")

##
## plotting data
##

plot(a)
plot(r)

##
## getting all years
##

# 2016
r2016 <- raster("SENTINEL2_TOA_NDVI_2016-08-12_2016-08-14_38p2464to38p2544N_-97p5563to-97p5459E.NDVI.tif")
plot(r2016)
a1 <- crop(r2016, field_shape)
plot(a1)

# 2017
r2017 <- raster("SENTINEL2_TOA_NDVI_2017-08-20_2017-08-22_38p2464to38p2544N_-97p5563to-97p5459E.NDVI.tif")
plot(r2017)
a2 <- crop(r2017, field_shape)
plot(a2)

# 2018
r2018 <- raster("SENTINEL2_TOA_NDVI_2018-08-07_2018-08-09_38p2464to38p2544N_-97p5563to-97p5459E.NDVI.tif")
plot(r2018)
a3 <- crop(r2018, field_shape)
plot(a3)

# 2019
r2019 <- raster("SENTINEL2_TOA_NDVI_2019-08-27_2019-08-29_38p2464to38p2544N_-97p5563to-97p5459E.NDVI.tif")
plot(r2019)
a4 <- crop(r2019, field_shape)
plot(a4)

# 2020
r2020 <- raster("SENTINEL2_TOA_NDVI_2020-05-16_2020-05-18_38p2464to38p2544N_-97p5563to-97p5459E.NDVI.tif")
plot(r2020)
a5 <- crop(r2020, field_shape)
plot(a5)

# 2021
r2021 <- raster("SENTINEL2_TOA_NDVI_2021-08-19_2021-08-21_38p2464to38p2544N_-97p5563to-97p5459E.NDVI.tif")
plot(r2021)
a6 <- crop(r2021, field_shape)
plot(a6)

# 2022
r2022 <- raster("SENTINEL2_TOA_NDVI_2023-08-11_2023-08-13_45p7197to45p7276N_-98p4260to-98p4137E.NDVI.tif")
plot(r2022)
a7 <- crop(r2022, field_shape)
plot(a7)


##
## plot of clean data
##

par(mfrow = c(3,3))
plot(a1)
plot(a2)
plot(a3)
plot(a4)
plot(a5)
plot(a6)
plot(a7)
