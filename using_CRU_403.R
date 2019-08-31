rm(list = ls())

setwd("C:/R/bachelorproject")

## importing the CRU data v4.03 (all)


#set path and file name

ncpath <- "C:/R/bachelorproject/data/cru_data/"
ncname <- "cru_ts4.03.1901.2018.tmp.dat"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "tmp"

library(ncdf4)

#open netCDF file

cru_all <- nc_open(ncfname)
cru_all

#get lon + lat

lon <- ncvar_get(cru_all,"lon")
nlon <- dim(lon)
lat <- ncvar_get(cru_all,"lat")
nlat <- dim(lat)

print(c(lon,lat))

# get time

time <- ncvar_get(cru_all, "time")
time

tunits <- ncatt_get(cru_all, "time", "units")
tunits
nt <- dim(time)
nt

# get temperature

tmp_array <- ncvar_get(cru_all, dname)
dlname <- ncatt_get(cru_all, dname, "long_name")
dunits <- ncatt_get(cru_all, dname, "units")
fillvalue <- ncatt_get(cru_all, dname, "_FillValue")
dim(tmp_array)

nc_close(cru_all)

## now convert into easy use format

library(chron)

# convert time , strgsplit

tustr <- strsplit(tunits$value, " ")
tustr

tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time, origin. = c(tmonth,tday,tyear), format = c(dates="m/d/year"))

# replace netCDF fillvalues with NA's

tmp_array[tmp_array==fillvalue$value] <- NA

## create the relevant data frame -- reshape data

#matrix nlon*nlat rows of 2 columns (lon + lat)

lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

#reshape the array into a vector
tmp_vec <- as.vector(tmp_array)
length(tmp_vec)

#reshape this vector into a matrix

tmp_mat <- matrix(tmp_vec, nrow = nlon*nlat, ncol=nt)
dim(tmp_mat)

head(na.omit(tmp_mat)) #okay this looks not like it should i think but let's try

tmp_all_df <- data.frame(cbind(lonlat, tmp_mat))

#rename col names
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(tmp_all_df)[1:2] <- paste(c("lon","lat"))
names(tmp_all_df) [3:ncol(tmp_all_df)] <- paste(rep(years, each=12), rep(month, times=118))

str(tmp_all_df)

#drop irrelevant columns , leaving only 1981-2002

library(tidyverse)
years1 <- 1981:2002

removecols <- paste(rep(years1, each=12), rep(month, times=22))
head(remcol,13)
tail(remcol,13)
head(tmp_all_df,13)
tail(tmp_all_df,13)
tmp_red_df <- tmp_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the tmp that we need: 1981-2002
head(tmp_red_df,13) #looks about right

##now import the country geodata through the GADM shapefile

library(rgdal)
#import on country level

gadmshape0 <- readOGR(dsn = "./data/gadm/gadm36_0.shp", layer = "gadm36_0")
class(gadmshape0)
head(gadmshape0)

#delete non-african countries

iso3afr <- c("DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","COM","COD","DJI","EGY","GNQ","ERI","ETH","GAB","GMB","GHA","GIN","GNB","CIV","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NAM","NER","NGA","COG","REU","RWA","SHN","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA","ESH","ZMB","ZWE")

gadmshape0afr <- gadmshape0[gadmshape0$GID_0 %in% iso3afr,]

glimpse(gadmshape0afr)
head(gadmshape0afr)


