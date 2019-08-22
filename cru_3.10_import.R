rm(list=ls())

library(ncdf4)
library(raster)

cru_3 <- nc_open("cru_ts_3_10.1901.2009.tmp.dat.nc")


print(cru_3)

tmp <- brick("cru_ts_3_10.1901.2009.tmp.dat.nc", varname="tmp")
