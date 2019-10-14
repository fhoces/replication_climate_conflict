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
gpcp_all <- read0GR(ncfname, layer = )
gpcp_all <- ncdf4::nc_open(ncfname)
gpcp_all

proj4string(gpcp_all)
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

##import the country geodata through the GADM shapefile
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. RECORDED FIRST HERE. I USE THE GADM DATASET AS A SOURCE FOR COUNTRY GRID INFORMATION. 
#UNFORTUNATELY AUTHORS DIDN'T DEFINE, WHICH SOURCE THEY ARE USING.

#import on country level
shpurl <- "https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_shp.zip"
shppath <- "./data/gadm/"
shpname <- "gadm36_0"
shpfname <- paste(shppath, shpname, ".shp", sep = "")

if(!file.exists(shpfname)) {
  
  temp_shp <- fs::file_temp(ext = ".shp")
  download.file(shpurl, destfile = temp_shp)
  unzip(zipfile = temp_shp, exdir = shppath)
  
  
}

del <- grep(list.files(shppath, full.names = T), pattern = shpname, invert = T, value = T)
file.remove(del)

gadmshape0 <- readOGR(dsn = "./data/gadm/gadm36_0.shp", layer = "gadm36_0")
class(gadmshape0)
summary(gadmshape0)
gadmshape0$NAME_0
gadmshape0$GID_0

#catch dataframe of iso codes and name
countrypolygons.df <- as.data.frame(gadmshape0)
str(countrypolygons.df)
gadmiso <- as.character(countrypolygons.df$GID_0)
#delete non-african countries

#ANALYTICAL CHOICE OF TYPE DATA SUB-SETTING. RECORDED FOR FIRST TIME HERE.
#analytical choice: defining african countries as the ones that are existent in the analytical dataset in the original replication files, which are 43 in total . 
#as of yet I do not know why this selection was made. 
#another choice could be: defining african countries as members of UN, which are 54, 
#this would leave out two african states: Reunion (part of France) and West Sahara (disputed) from the 56 in total african states
#and would be achieved by iso3afralternative <- c( "DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","COM","COD","DJI",
#                                                   "EGY","GNQ","ERI","ETH","GAB","GMB","GHA","GIN","GNB","CIV","KEN","LSO",
#                                                   "LBR","LBY","MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NAM","NER","NGA",
#                                                   "COG","RWA","SHN","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN","SWZ",
#                                                   "TZA","TGO","TUN","UGA","ZMB","ZWE")

iso3afr <- c("GNB", "GMB", "MLI", "SEN", "BEN", "MRT", "NER", "CIV", "GIN", 
             "BFA", "LBR", "SLE", "GHA", "TGO", "CMR", "NGA", "GAB","CAF", 
             "TCD", "COG", "ZAR", "UGA", "KEN", "TZA", "BDI", "RWA", 
             "SOM","DJI","ETH","AGO","MOZ","ZMB","ZWE","MWI","ZAF",
             "NAM","LSO","BWA","SWZ","MDG","SDN")

#is there any iso code that's defined for our african country vector but not in gadmshape?

iso3afr[!iso3afr %in% gadmiso]

#answer is yes: ZAR .. this is former Zaire, now congo, demoocratic republic with the iso code COD
#redefining this single country

iso3afr <- c("GNB", "GMB", "MLI", "SEN", "BEN", "MRT", "NER", "CIV", "GIN",
             "BFA", "LBR", "SLE", "GHA", "TGO", "CMR", "NGA", "GAB","CAF",
             "TCD", "COG", "COD", "UGA", "KEN", "TZA", "BDI", "RWA", 
             "SOM","DJI","ETH","AGO","MOZ","ZMB","ZWE","MWI","ZAF",
             "NAM","LSO","BWA","SWZ","MDG","SDN")

#let's check

iso3afr[!iso3afr %in% gadmiso]

#gives out NULL , so we're good to go now

gadmshape0afr <- gadmshape0[gadmshape0$GID_0 %in% iso3afr,]

summary(gadmshape0afr)
rm(gadmshape0)


##get tmp data for the single countries

#delete irrelevant grids in the tmp_red_df dataframe
summary(gadmshape0afr)
#we see in gadmshape0afr for which extent data is defined for a country

gpcp_redafr_df <- subset(gpcp_red_df, lon >= -17.75 & lon <=51.5 & lat >= -35 & lat <= 27.5)
head(gpcp_redafr_df)

#rename the rows
row.names(gpcp_redafr_df) <- c(1:nrow(gpcp_redafr_df))

#turn gpcp_red_df into Spatialpoints

gpcp_coords <- cbind(gpcp_redafr_df$lon, gpcp_redafr_df$lat)
gpcp_pts <- SpatialPoints(coords = gpcp_coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
gpcp_pts

#get the location for the grid cells in tmp dataframe

loc_tmps <- tmp_pts %over% gadmshape0afr
na.omit(loc_tmps)
summary(loc_tmps)

##merging the temperature (incl. lon +lat) with the country codes

str(loc_tmps)
str(tmp_redafr_df)
head(na.omit(loc_tmps))

full_tmp <- bind_cols(loc_tmps, tmp_redafr_df)
dim(full_tmp)

#delete obs. with either no tmp-data or not defined for an african country

full_tmp <- na.omit(full_tmp)
dim(full_tmp)
head(full_tmp)

#NAME_0 is unnecessary, and we won't need lon and lat anymore

full_tmp <- full_tmp %>% select(-NAME_0, -lon, -lat)

