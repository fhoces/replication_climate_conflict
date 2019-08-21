## Importing GADM shapefile

rm(list=ls())
setwd("C:/R/bachelorproject/RASTER countries/shapefile GADM")
library(rgdal)
library(spdep)
library(tidyverse)

#import on country level

gadmshape0 <- readOGR(dsn = "./gadm36_levels_shp/gadm36_0.shp", layer = "gadm36_0")
class(gadmshape0)
head(gadmshape0)

#delete non-african countries

iso3afr <- c("DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","COM","COD","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA","GIN","GNB","CIV","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NAM","NER","NGA","COG","REU","RWA","SHN","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ESH","ZMB","ZWE")

gadmshape0afr <- gadmshape0[gadmshape0afr$GID_0 %in% iso3afr,]

head(gadmshape0afr)

