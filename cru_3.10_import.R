rm(list=ls())

library(ncdf4)
library(raster)

cru_3 <- nc_open("cru_ts_3_10.1901.2009.tmp.dat.nc")

{sink("cru_ts_3_10.1901.2009.tmp.dat.nc")
  print(cru_3)
    sink()
    
}

summary(cru_3)
dim(cru_3)

lon <- ncvar_get(cru_3, "lon")
lat <- ncvar_get(cru_3, "lat")
t <- ncvar_get(cru_3, "time")

head(lon)
head(lat)
sum(lat)
summary(lat)
