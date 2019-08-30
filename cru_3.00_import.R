rm(list=ls())

library(ncdf4)
library(raster)
library(maps)
library(rasterVis)
library(maptools)

setwd("C:/R/bachelorproject/cruimport")
file.exists("cru_ts_3_00.1901.2006.tmp.nc")
cru_3 <- nc_open("cru_ts_3_00.1901.2006.tmp.nc")

print(cru_3)
tmp <- brick("cru_ts_3_00.1901.2006.tmp.nc", varname="tmp")

tmp

print(tmp)

xFromCol(tmp)       #running from 179.75 to -179.75 --> because raster uses center of grids
yFromRow(tmp)       #same her , from 89.75 to-89.75

plot(tmp$X372)
