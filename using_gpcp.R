#using GPCP 

##set up

rm(list = ls())

setwd("C:/R/bachelorproject")

#load packages
package_load <- function(x) {
  for (i in x) {
    # require returns TRUE invisibly if it was able to load packages
    if ( ! require (i, character.only = TRUE)) {
      # IF package was not able to be loaded ten re-install
      install.packages(i, dependencies = T)
      
    }
    
  }
}

packages <- c("ncdf4","tidyverse", "chron", "rgdal", "readxl", "splitstackshape", "fastDummies",
              "wbstats", "pwt","pwt9", "data.table", "foreign", "plm", "stargazer", "R.utils")

package_load(packages)

## importing the GPCP data v 2.3 (all)

#set path and file name

ncurl <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcp/precip.mon.mean.nc"
ncpath <- "C:/R/bachelorproject/data/gpcp/"
ncname <- "precip.mon.mean"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "precip"

#open netCDF file

gpcp_all <- ncdf4::nc_open(ncfname)
gpcp_all

#get lon + lat

lon <- ncvar_get(gpcp_all,"lon")
nlon <- dim(lon)
lat <- ncvar_get(gpcp_all,"lat")
nlat <- dim(lat)

print(c(lon,lat))

# get time

time <- ncvar_get(gpcp_all, "time")
time

tunits <- ncatt_get(gpcp_all, "time", "units")
tunits
nt <- dim(time)
nt

# get temperature

gpcp_array <- ncvar_get(gpcp_all, dname)
dunits <- ncatt_get(gpcp_all, dname, "units")
fillvalue <- ncatt_get(gpcp_all, dname, "missing_value")
dim(gpcp_array)

nc_close(gpcp_all)


## now convert into easy use format

# convert time , strgsplit

tustr <- strsplit(tunits$value, " ")
tustr

tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time, origin. = c(tmonth,tday,tyear), format = c(dates="m/d/year")) #just to have a look

# replace netCDF fillvalues with NA's

gpcp_array[gpcp_array==fillvalue$value] <- NA
head(gpcp_array)


## create the relevant data frame -- reshape data

#matrix nlon*nlat rows of 2 columns (lon + lat)

lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

#reshape the array into a vector

gpcp_vec <- as.vector(gpcp_array)
length(gpcp_vec)

#reshape this vector into a matrix

gpcp_mat <- matrix(gpcp_vec, nrow = nlon*nlat, ncol=nt)
dim(gpcp_mat)

gpcp_all_df <- data.frame(lonlat, gpcp_mat)

rm(gpcp_array)
rm(gpcp_vec)
rm(gpcp_mat)

#rename col names

yearsgpcp <- 1979:2019
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(gpcp_all_df)[1:2] <- paste(c("lon","lat"))
names(gpcp_all_df)[3:(ncol(gpcp_all_df))] <- paste(rep(yearsgpcp, each=12), rep(month, times = length(yearsgpcp)))

str(gpcp_all_df)


#drop irrelevant columns , leaving only 1979 - 2006
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED HERE.

years1 <- 1979:2006

removecols1 <- c("lon","lat")
removecols2 <- paste(rep(years1, each=12), rep(month, times= length(years1)))
removecols <- c(removecols1,removecols2)
head(removecols,13)
tail(removecols,13)
head(gpcp_all_df)

gpcp_red_df <- gpcp_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the pre at the time we need: 1979-2002
head(gpcp_red_df)
head(na.omit(gpcp_red_df))

rm(gpcp_all_df)