## Importing GADM shapefile

rm(list=ls())
setwd("C:/R/bachelorproject/RASTER countries/shapefile GADM")
library(rgdal)

#import on country level
gadmshape0 <- readOGR(dsn = "./gadm36_levels_shp/gadm36_0.shp", layer = "gadm36_0")

  

