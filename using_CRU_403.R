### master file - replicating the results of the paper "Warming increases the risk of civil war in Africa" , using an updated climate dataset version

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

## importing the CRU data v4.03 (all)

#set path and file name

ncurl <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc.gz"
ncpath <- "C:/R/bachelorproject/data/cru_data/"
ncname <- "cru_ts4.03.1901.2018.tmp.dat"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "tmp"

if(!file.exists(ncfname)){
  temp_nc <- fs::file_temp(ext = ".nc.gz")
  download.file(ncurl, destfile = temp_nc, mode = "wb")
  suppressMessages(library(R.utils))
  
  gunzip(filename = temp_nc, destname = ncfname)
  
}

#open netCDF file

cru_all <- ncdf4::nc_open(ncfname)
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
dunits <- ncatt_get(cru_all, dname, "units")
fillvalue <- ncatt_get(cru_all, dname, "_FillValue")
dim(tmp_array)

nc_close(cru_all)


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

tmp_array[tmp_array==fillvalue$value] <- NA
head(tmp_array)


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

tmp_all_df <- data.frame(lonlat, tmp_mat)

rm(tmp_array)
rm(tmp_vec)
rm(tmp_mat)

#rename col names

years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(tmp_all_df)[1:2] <- paste(c("lon","lat"))
names(tmp_all_df)[3:ncol(tmp_all_df)] <- paste(rep(years, each=12), rep(month, times = length(years)))

str(tmp_all_df)

#drop irrelevant columns , leaving only 1979 - 2006
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED HERE.
#I include years 1979 - 1980 to compute climate lags and climate_diff lags. 
#From the original data, I can not see how they computed the lag variable for 1981, but I assume this is how they did it , too.

years1 <- 1979:2006

removecols1 <- c("lon","lat")
removecols2 <- paste(rep(years1, each=12), rep(month, times= length(years1)))
removecols <- c(removecols1,removecols2)
head(removecols,13)
tail(removecols,13)
head(tmp_all_df)

tmp_red_df <- tmp_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the tmp at the time we need: 1979-2002
head(tmp_red_df)
head(na.omit(tmp_red_df))

rm(tmp_all_df)


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

tmp_redafr_df <- subset(tmp_red_df, lon >= -17.75 & lon <=51.5 & lat >= -35 & lat <= 27.5)
head(tmp_redafr_df)

#rename the rows
row.names(tmp_redafr_df) <- c(1:nrow(tmp_redafr_df))

#turn tmp_red_df into Spatialpoints

tmp_coords <- cbind(tmp_redafr_df$lon, tmp_redafr_df$lat)
tmp_pts <- SpatialPoints(coords = tmp_coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
tmp_pts

#get the location for the grid cells in tmp dataframe

loc_tmps <- tmp_pts %over% gadmshape0afr
dim(na.omit(loc_tmps))
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

  
## the temperature is first averaged over the grid cells in a country, then over the month of a year
# ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED HERE.

#averaging over cells in country

country_tmp <- aggregate(full_tmp[2:ncol(full_tmp)], list(full_tmp$GID_0), mean)
names(country_tmp)[1] <- "iso3"
country_tmp

# restructuring the table -> we want the tmp information for the month not in columns but in one column , each obs. being a row

country_tmp_num <- subset(country_tmp, select = -iso3) #need this subset with only the numerical values to use function ; _num stands for numerical
tmp_vec1 <- as.vector(t(country_tmp_num))
length(tmp_vec1)
iso3 <- subset (country_tmp, select = iso3)
iso3 <- as.vector(t(iso3))
iso3rep <- unlist(rep(iso3,each = ncol(full_tmp)-1))  #countrycodes for all the 264 obs. of tmp per country (12m*22y)
summary(iso3rep)
iso3rep
colnames1 <- colnames(country_tmp_num)

country_tmp <- data.frame(iso3rep,colnames1,tmp_vec1, stringsAsFactors = FALSE) 

colnames(country_tmp) <- c("iso3","months", "tmp") 
summary(country_tmp)
dim(country_tmp)
# calculate yearly data

country_tmp_ann <- country_tmp %>% separate(months, into=c("years", "months")) %>% group_by(iso3, years)%>% summarise(tmp=mean(tmp))

dim(country_tmp_ann)
table(country_tmp_ann$years)
table(country_tmp_ann$iso3)

# create lag, lead and quadratic term

country_tmp_ann <- country_tmp_ann %>% mutate(tmp_lag = dplyr::lag(tmp), 
                                              tmp_lead = dplyr::lead(tmp),
                                              tmp_square = tmp^2,
                                              tmp_lag_square = tmp_lag^2)
# calculate diff and dev

# ANALTYCAL CHOICE OF TYPE VARIABLE DEFINITION. FIRST RECORDED HERE.
# we define climate difference in year t as difference between climate in year t and year t-1
# this is assumed to be the meassure, even tho controlling in the original replication file doesn't confirm this 100%, maybe due to rouding error

#diff
country_tmp_ann <- country_tmp_ann %>% mutate(tmp_diff = tmp - tmp_lag,
                                              tmp_diff_lag = dplyr::lag(tmp_diff))
                                             
#dev

#estimate trend for tmp
#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITON. FIRST RECORDED HERE.
# I define the trend as a countryspefic, linear trend estimation with one regressor plus intercept 
# during the years 1981 - 2002. The authors seem to have been using a different dataset, but as I'm using another 
# dataset version I can not be certain which.

country_tmp_ann <- country_tmp_ann %>% filter(years >= 1981, years <= 2002)

trendcoef <- country_tmp_ann %>%
  group_by(iso3) %>% 
  do(model_lin_tmp = lm(tmp ~ years, .)) %>%
  ungroup()

trendcoef

#use these estimates to compute predictions for all obs.

country_tmp_ann <- country_tmp_ann %>% left_join(trendcoef, by = "iso3")

country_tmp_ann <- country_tmp_ann %>% 
  group_by(iso3) %>% 
  do(modelr::add_predictions(., first(.$model_lin_tmp), var = "pred_lin_tmp"))

country_tmp_ann <- country_tmp_ann %>% mutate(tmp_difftrend = tmp - pred_lin_tmp)
country_tmp_ann <- country_tmp_ann %>% select(-model_lin_tmp, -pred_lin_tmp) 

view(country_tmp_ann)
### temperature finished

write_csv(country_tmp_ann, "C:/R/bachelorproject/csv_files/country_tmp_ann.csv")

##importing precipitation


ncurl <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/cruts.1905011326.v4.03/pre/cru_ts4.03.1901.2018.pre.dat.nc.gz"
ncpath <- "C:/R/bachelorproject/data/cru_data/"
ncname <- "cru_ts4.03.1901.2018.pre.dat"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "pre"

if(!file.exists(ncfname)){
  temp_nc <- fs::file_temp(ext = ".nc.gz")
  download.file(ncurl, destfile = temp_nc, mode = "wb")
  suppressMessages(library(R.utils))
  
  gunzip(filename = temp_nc, destname = ncfname)
  
}

#open netCDF file

cru_all_pre <- nc_open(ncfname)
cru_all_pre

# get precipitation

pre_array <- ncvar_get(cru_all_pre, dname)
dunits <- ncatt_get(cru_all_pre, dname, "units")
fillvalue <- ncatt_get(cru_all_pre, dname, "_FillValue")
dim(pre_array)

nc_close(cru_all_pre)

# replace netCDF fillvalues with NA's

pre_array[pre_array==fillvalue$value] <- NA
head(pre_array)


## create the relevant data frame -- reshape data

#reshape the array into a vector

pre_vec <- as.vector(pre_array)
length(pre_vec)

#reshape this vector into a matrix

pre_mat <- matrix(pre_vec, nrow = nlon*nlat, ncol=nt)
dim(pre_mat)

pre_all_df <- data.frame(cbind(lonlat, pre_mat))

rm(pre_array)
rm(pre_vec)
rm(pre_mat)

#rename col names

names(pre_all_df)[1:2] <- paste(c("lon","lat"))
names(pre_all_df)[3:ncol(pre_all_df)] <- paste(rep(years, each=12), rep(month, times= length(years)))

str(pre_all_df)

#drop irrelevant columns , leaving only 1979-2002
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED IN LINE 133

pre_red_df <- pre_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the tmp at the time we need: 1979-2002
head(tmp_red_df)
head(na.omit(tmp_red_df))

rm(pre_all_df)

##get pre data for the single countries

#delete irrelevant grids in the pre_red_df dataframe
#we see in gadmshape0afr for which extent we need data for

pre_redafr_df <- subset(pre_red_df, lon >= -17.75 & lon <=51.5 & lat >= -35 & lat <= 27.5)
head(pre_redafr_df)

#rename the rows
row.names(pre_redafr_df) <- c(1:nrow(pre_redafr_df))

#turn pre_red_df into Spatialpoints

pre_coords <- cbind(pre_redafr_df$lon, pre_redafr_df$lat)
pre_pts <- SpatialPoints(coords = pre_coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
pre_pts

#get the location for the grid cells in pre dataframe

loc_pre <- pre_pts %over% gadmshape0afr
na.omit(loc_pre)
summary(loc_pre)

##merging the precipitation (incl. lon +lat) with the country codes

str(loc_pre)
str(pre_redafr_df)
head(na.omit(loc_pre))

full_pre <- bind_cols(loc_pre, pre_redafr_df)
dim(full_pre)

#delete obs. with either no pre-data or not defined for an african country

full_pre <- na.omit(full_pre)
dim(full_pre)
head(full_pre)

#NAME_0 is unnecessary, and we won't need lon and lat anymore

full_pre <- full_pre %>% select(-NAME_0, -lon, -lat)


## the precipitation is first averaged over the grid cells in a country, then over the month of a year


#averaging over cells in country

country_pre <- aggregate(full_pre[2:ncol(full_pre)], list(full_pre$GID_0), mean)
names(country_pre)[1] <- "iso3"
country_pre

# restructuring the table -> we want the pre information for the month not in columns but in one column , each obs. being a row

country_pre_num <- subset(country_pre, select = -iso3) #need this subset with only the numerical values to use function ; _num stands for numerical
pre_vec1 <- as.vector(t(country_pre_num))
length(pre_vec1)

country_pre <- data.frame(iso3rep,colnames1,pre_vec1,stringsAsFactors = FALSE) 
str(country_pre)
colnames(country_pre) <- c("iso3","months", "pre") 

# calculate yearly data

country_pre_ann <- country_pre %>% separate(months, into=c("years", "months")) %>% group_by(iso3, years) %>% summarise(pre=mean(pre))

dim(country_pre_ann)

#adjust unit : divide by 100

country_pre_ann$pre <- country_pre_ann$pre/100

#create lag, lead and quadratic term

country_pre_ann <- country_pre_ann %>% mutate(pre_lag = dplyr::lag(pre), 
                                              pre_lead = dplyr::lead(pre),
                                              pre_square = pre^2,
                                              pre_lag_square = pre_lag^2)

#create diff and dev from trend
# ANALTYCAL CHOICE OF TYPE VARIABLE DEFINITION. FIRST RECORDED IN LINE 313. 

country_pre_ann <- country_pre_ann %>% mutate(pre_diff = pre - pre_lag,
                                              pre_diff_lag = dplyr::lag(pre_diff))


#dev

#estimate trend for tmp
#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITON. FIRST RECORDED IN LINE 326.


trendcoef <- country_pre_ann %>% 
  filter(years >=1981, years <= 2002) %>%
  group_by(iso3) %>% 
  do(model_lin_pre = lm(pre ~ years, .)) %>%
  ungroup()

trendcoef

#use these estimates to compute predictions for all obs.

country_pre_ann <- country_pre_ann %>% left_join(trendcoef, by = "iso3")

country_pre_ann <- country_pre_ann %>% 
  filter(years >=1981, years <= 2002) %>% 
  group_by(iso3) %>% 
  do(modelr::add_predictions(., first(.$model_lin_pre), var = "pred_lin_pre"))

country_pre_ann <- country_pre_ann %>% mutate(pre_difftrend = pre - pred_lin_pre)
country_pre_ann <- country_pre_ann %>% select(-model_lin_pre, -pred_lin_pre) 


view(country_pre_ann)

#write into file

write_csv(country_pre_ann, "C:/R/bachelorproject/csv_files/country_pre_ann.csv")


### CRU precipitation finished

### import GPCP precipitation

gpcpurl <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcp/precip.mon.mean.nc"
gpcppath <- "./data/gpcp/"
gpcpname <- "precip.mon.mean.nc"
gpcpfname <- paste(gpcppath, gpcpname, sep = "")
dname <- "precip"

if(!file.exists(gpcpfname)) {
  
  download.file(gpcpurl, gpcpfname, mode = "wb")
  
}

#open netCDF file

gpcp <- nc_open(gpcpfname)
gpcp


#get lon + lat

lon <- ncvar_get(gpcp,"lon")
nlon <- dim(lon)

lat <- ncvar_get(gpcp,"lat")
nlat <- dim(lat)

print(c(lon,lat)) #lon is from 0-360, we want it -180 to 180

lon <- lon-180
# get time

time <- ncvar_get(gpcp, "time")
time

tunits <- ncatt_get(gpcp, "time", "units")
tunits
nt <- dim(time)
nt

# get precipitation

gpcp_array <- ncvar_get(gpcp, dname)
dunits <- ncatt_get(gpcp, dname, "units")
fillvalue <- ncatt_get(gpcp, dname, "missing_value")
dim(gpcp_array)

nc_close(gpcp)


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
summary(gpcp_all_df)
rm(gpcp_array)
rm(gpcp_vec)
rm(gpcp_mat)

#rename col name

years <- 1979:2019
names(gpcp_all_df)[1:2] <- paste(c("lon","lat"))
names(gpcp_all_df)[3:ncol(gpcp_all_df)] <- paste(rep(years, each=12), rep(month, times = length(years)))

str(gpcp_all_df)

#drop irrelevant columns , leaving only 1979 - 2006
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED IN LINE 133

gpcp_red_df <- gpcp_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the gpcp at the time we need: 1979-2002
head(gpcp_red_df)
head(na.omit(gpcp_red_df))

rm(gpcp_all_df)

##get pre data for the single countries

#delete irrelevant grids in the gpcp_red_df dataframe
#we see in gadmshape0afr for which extent we need data for

gpcp_redafr_df <- subset(gpcp_red_df, lon >= -17.75 & lon <=51.5 & lat >= -35 & lat <= 27.5)
head(gpcp_redafr_df)

#rename the rows
row.names(pre_redafr_df) <- c(1:nrow(pre_redafr_df))

#get gpcp Spatialpoints

gpcp_coords <- cbind(gpcp_redafr_df$lon, gpcp_redafr_df$lat)
gpcp_pts <- SpatialPoints(coords = gpcp_coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
gpcp_pts

#get the location for the grid cells in pre dataframe

loc_gpcp <- gpcp_pts %over% gadmshape0afr
na.omit(loc_gpcp)
summary(loc_gpcp) # there's very few obs... why?


##merging the precipitation (incl. lon +lat) with the country codes

str(loc_gpcp)
str(gpcp_redafr_df)
head(na.omit(loc_gpcp))

full_gpcp <- bind_cols(loc_gpcp, gpcp_redafr_df)
dim(full_gpcp)

#delete obs. with either no pre-data or not defined for an african country

full_gpcp <- na.omit(full_gpcp)
dim(full_gpcp)
head(full_gpcp)

#NAME_0 is unnecessary, and we won't need lon and lat anymore

full_gpcp <- full_gpcp %>% select(-NAME_0, -lon, -lat)

## the precipitation is first averaged over the grid cells in a country, then over the month of a year


#averaging over cells in country

country_gpcp <- aggregate(full_gpcp[2:ncol(full_gpcp)], list(full_gpcp$GID_0), mean)
names(country_gpcp)[1] <- "iso3"

# restructuring the table -> we want the pre information for the month not in columns but in one column , each obs. being a row


country_gpcp_num <- subset(country_gpcp, select = -iso3) #need this subset with only the numerical values to use function ; _num stands for numerical
gpcp_vec1 <- as.vector(t(country_gpcp_num))
length(gpcp_vec1)

iso3 <- subset (country_gpcp, select = iso3)
iso3 <- as.vector(t(iso3))
iso3rep <- unlist(rep(iso3,each = ncol(full_gpcp)-1))  #countrycodes for all the 264 obs. of tmp per country (12m*22y)
summary(iso3rep)
iso3rep
colnames2 <- colnames(country_gpcp_num)

country_gpcp <- data.frame(iso3rep,colnames2,gpcp_vec1,stringsAsFactors = FALSE) 
str(country_gpcp)
colnames(country_gpcp) <- c("iso3","months", "pre") 

# calculate yearly data

country_gpcp_ann <- country_gpcp %>% separate(months, into=c("years", "months")) %>% group_by(iso3, years) %>% summarise(gpcp=mean(pre))

dim(country_gpcp_ann)

#adjust unit : divide by 100

country_gpcp_ann$pre <- country_gpcp_ann$pre/100

#write into file

write_csv(country_gpcp_ann, "C:/R/bachelorproject/csv_files/country_gpcp_ann.csv")

### GPCP finished

### import conflict data

xlsurl <- "https://www.prio.org/Global/upload/CSCW/Data/UCDP/2008/MainConflictTable.xls"
xlspath <- "./data/conflict/"
xlsname <- "MainConflictTable.xls"
xlsfname <- paste(xlspath, xlsname, sep = "")

if(!file.exists(xlsfname)) {
  
  download.file(xlsurl, xlsfname, mode = "wb")
}

conflict <- read_xls(xlsfname)

## filter down to relevant data

#delete obs. not between 1981 and 2006

conflict <- conflict %>% filter(YEAR >= 1979 & YEAR <=2006)

conflict
#observations in african countries

table(conflict$Location)

#duplicate the rows which have multiple locations 

conflict <- cSplit(conflict, "SideA", sep = ",", direction = "long", type.convert = "as.character")

#rename and define the incidence of conflict as conflict in the countries territory -> this seems to be what burke et al. did

#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. RECORDED FIRST HERE.
# we use the location as the indice for a war , not e.g. SideA indicator in PRIO dataset

conflict <- conflict %>% rename(countryname = SideA)

#get countrynames from burke

##ANALYTICAL CHOICE OF TYPE DATA SUB-SETTING. RECORDED FOR FIRST TIME IN LINE 144.

africancountries <- data.frame(iso3 = iso3afr,
                               countryname = c("Guinea-Bissau", 
                                               "Gambia, The", 
                                               "Mali", 
                                               "Senegal", 
                                               "Benin", 
                                               "Mauritania", 
                                               "Niger", 
                                               "Cote d'Ivoire", 
                                               "Guinea", 
                                               "Burkina Faso", 
                                               "Liberia", 
                                               "Sierra Leone", 
                                               "Ghana", 
                                               "Togo" ,
                                               "Cameroon", 
                                               "Nigeria", 
                                               "Gabon", 
                                               "Central African Republic", 
                                               "Chad", 
                                               "Congo, Republic of", 
                                               "Congo, Dem. Rep.", 
                                               "Uganda", 
                                               "Kenya", 
                                               "Tanzania", 
                                               "Burundi",
                                               "Rwanda", 
                                               "Somalia", 
                                               "Djibouti", 
                                               "Ethiopia", 
                                               "Angola", 
                                               "Mozambique", 
                                               "Zambia", 
                                               "Zimbabwe", 
                                               "Malawi", 
                                               "South Africa", 
                                               "Namibia", 
                                               "Lesotho", 
                                               "Botswana", 
                                               "Swaziland", 
                                               "Madagascar", 
                                               "Sudan"),
                                                stringsAsFactors = FALSE) #these are from the original replication analytic data
                                                                                                                                                    
              
#let's see which countries are in conflict table that we didn't define

unique(conflict$countryname[!conflict$countryname %in% africancountries$countryname]) #are in PRIO but not in our african countryset

#the result are these (narrowed to african countries) , which might have been just different spelled or defined in burke dataset: 
#c( "Yemen (Arab Republic of Yemen)", "Democratic Republic of Congo (Zaire)", "Morocco", "Mozambique", "Gambia", 
#   "Democratic Republic of Yemen", "People's Republic of Yemen", "Lybia", "Comoros", "Algeria", "Egypt", "Congo", "Cote D'Ivoire") 

#checking the other way around
africancountries$countryname[!africancountries$countryname %in% conflict$countryname]

#result : "Gambia, The" -->  relates to "Gambia" 
#         "Cote d'Ivoire" -->  relates to "Cote D'Ivoire"
#         "Congo, Dem. Rep." --> relates to "Democratic Republic of Congo (Zaire)"
#         "Zambia" --> not recorded in PRIO
#         "Namibia" --> no war recorded in PRIO for relevant time period
#         "Madagascar" --> no war recorded in PRIO for relevant time period
#         "Benin" --> not recorded in PRIO
#         "Gabon" --> not recorded in PRIO
#         "Tanzania" --> no war recorded in PRIO for relevant time period
#         "zimbabwe" -->  relates to "Zimbabwe (Rhodesia)"
#         "Botswana" --> no war recorded in PRIO for relevant time period
#         "Mauritania" --> no war recorded in PRIO for relevant time period
#         "Congo, Republic of" --> relates to "Congo"? different war record in both tables
#         "Malawi" --> not recorded in PRIO
#         "Swaziland" --> not recorded in PRIO

#let's redefine (we redefine it in the conflict table for easier comparability to original results)

conflict$countryname <- recode(conflict$countryname,
                               "Gambia" = "Gambia, The",
                               "Cote D'Ivoire" = "Cote d'Ivoire", 
                               "Democratic Republic of Congo (Zaire)" = "Congo, Dem. Rep.",
                               "Zimbabwe (Rhodesia)" = "Zimbabwe", 
                               "Congo" = "Congo, Republic of" )

africancountries$countryname[!africancountries$countryname %in% conflict$countryname]


#subset conflict data
##ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. RECORDED FIRST IN LINE 438.
#if changing the variable defintion for conflict, e.g. not being location but SideA, then needs to be changed here too.
#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. RECORDED FIRST HERE.
#in addition to using location , we require intensity to be 2 (meaning >1k deaths, like defined in supporting information)
conflict <- conflict %>% 
  filter(countryname %in% africancountries$countryname & Int == 2) %>% 
  select(countryname, YEAR)

#delete duplicates
#ANALYTICAL CHOICE MADE OF TYPE VARIABLE DEFINITION. TREAT COUNTRIES WITH ONE CONFLICT SAME AS WITH MULTIPLE. FIRST RECORDED HERE.

conflict <- conflict %>% distinct()

#make dataframes ready for merger
  
conflict <- left_join(conflict, africancountries, by = c("countryname")) #create iso3 for countries
conflict <- conflict %>% rename("years" = "YEAR")
conflict[,2] <- as.character(conflict[,2])
conflict$conflict <- 1

africancountriesrep <- data.frame(iso3 = rep(africancountries$iso3, each = length(years1)), 
                                  countryname = rep(africancountries$countryname, each = length(years1)), 
                                  years = as.character(years1), 
                                  stringsAsFactors = F)

conflict <- right_join(conflict, africancountriesrep, by = c("countryname", "years", "iso3"))

###import of conflict finished for now

###merge tmp, pre and conflict

climate_conflict <- list(country_tmp_ann, country_pre_ann,country_gpcp_ann, conflict) %>% reduce(full_join, by = c("iso3", "years"))

climate_conflict$conflict[is.na(climate_conflict$conflict)] <- 0 #changes NA values in conflict to 0 (no conflict)

view(climate_conflict)


view(climate_conflict[is.na(climate_conflict$conflict),]) # no more NA's

# because of missing climate data, the data has been rearranged.. sort 

climate_conflict <- climate_conflict %>% arrange(iso3, years)
#create conflict onset variable

conflict_onset_rows <- which(climate_conflict$conflict == 1 & dplyr::lag(climate_conflict$conflict)==0 & dplyr::lag(climate_conflict$countryname) == climate_conflict$countryname) #creates rowIDs where conflict onsets
climate_conflict$conflict_onset <- 0
climate_conflict[conflict_onset_rows,]$conflict_onset <- 1 

#ANALYITAL CHOICE OF TYPE : VARIABLE DEFINITION. FIRST RECORDED HERE.
# we define onset variable "as missing if there was an ongoing civil war that began in an earlier year" (cited by manual)

climate_conflict$conflict_onset[climate_conflict$conflict == 1 & climate_conflict$conflict_onset == 0] <- NA


climate_conflict <- climate_conflict %>% filter(!years == 1979:1980) #only needed 1979 and 1980 to create the lag, as stated above

### write csv

write_csv(climate_conflict,"./csv_files/climate_conflict.csv")

###download control data

##GDP data
##NOTE : the authors state that they derive the GDP from World Bank AND PENN.
##as only one GDP meassure is used for the regression, it is very unclear to me,
##how the used GDP per capita has been derived.

#GDP from PENN
#ANALYTICAL CHOICE OF TYPE OTHERS - PROCESSING
#using pwt 6.2

data("pwt6.2")

pwt6.2$isocode <- as.character(pwt6.2$isocode)
pwt6.2$year <- as.character(pwt6.2$year)
pwt6.2$isocode[pwt6.2$isocode == "ZAR"] <- "COD"
pwt6.2 <- pwt6.2 %>% filter(year>= 1981, year<=2006) %>% select(iso3 = isocode, years = year, gdp = rgdptt)
pwt6.2$gdp <- pwt6.2$gdp/1000
climate_conflict <- left_join(climate_conflict, pwt6.2, by = c("iso3", "years"))

table(is.na(climate_conflict$gdp))#132 missing values


view(climate_conflict[is.na(climate_conflict$gdp) & climate_conflict$years <= 2002,]) #Angola -> like in original dataset

##Polity IV data 

politypath <- "./data/polity/"
polityfname <- "p4v2007.xls"
dest_polity <- paste(politypath, polityfname, sep = "") # not publicly available online, but upon request from Center for Systemic Peace
polity <- read_xls(dest_polity)
view(polity)

##scodes are different from iso3 codes, so we have to redefine them
polity <- as.data.frame(polity) #resolves issue with warning message : unknown or uninitialised column = "country"
polityjoin <- left_join(polity, africancountries, by= c("country" = "countryname"))
uniqueN(polityjoin$country[!is.na(polityjoin$iso3)]) #37 iso codes --> this is good, but seems like there's 4 left where countryname is different too

unique(polityjoin$country[is.na(polityjoin$iso3)])
#can we find african countries? 
#Cote D'Ivoire, Ivory coast, congo brazzaville (this is republic) , congo kinshasa (this is democratic republic), gambia, 

polity$country[polity$country == "Cote D'Ivoire"] <- "Cote d'Ivoire"
polity$country[polity$country == "Ivory Coast"] <- "Cote d'Ivoire"
polity$country[polity$country == "Congo Brazzaville"] <- "Congo, Republic of"
polity$country[polity$country == "Congo Kinshasa"] <- "Congo, Dem. Rep."
polity$country[polity$country == "Gambia"] <- "Gambia, The"


polityjoin <- left_join(polity, africancountries, by= c("country" = "countryname"))
uniqueN(polityjoin$country[!is.na(polityjoin$iso3)]) #41 iso codes --> good


##ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. FIRST RECORDED HERE.
##using polity2 as meassure of political system.

polityjoin <- polityjoin %>% filter(year >= 1981, year <= 2006) %>%
  select(years = year, iso3, polity2)
polityjoin$years <- as.character(polityjoin$years)

climate_conflict <- left_join(climate_conflict, polityjoin, by = c("iso3", "years")) ## we have one more obs. now .. what happened there?
table(climate_conflict$countryname) #ethiopia has 23 obs. instead of 22

#looking that up in the polityIV table shows they apparently changed the countrycode in 1993 and have two observations for that year
#quick wikipedia search shows they got a new constitution in 1994, probably has something to do with that
#it's behind my scope to decide which one is better.. but it changed only marginaly from 0 to 1 
#the authors decided to use the obs. with the value 0 (for unclear reason, but they were probably indifferent in their choice)
#ANALYTICAL CHOICE OF TYPE DATA-SUBSETTING. FIRST RECORDED HERE.

climate_conflict <- climate_conflict[!(climate_conflict$countryname == "Ethiopia" & climate_conflict$years == 1993 & climate_conflict$polity2 == 1),]



table(is.na(climate_conflict$polity2)) #9 missing values
polityNA <- climate_conflict[is.na(climate_conflict$polity2),]
view(polityNA) #Namibia politic score only starts in 1990

##

write_csv(climate_conflict,"./csv_files/climate_conflict.csv")


### adjust the constructed data table so that it matches the original one 


## delete country-year observations that are missing in original replication files
## ANALYTICAL CHOICE OF TYPE DATA SUB-SETTING. FIRST RECORDED HERE.
# I do not see the reason behind removing these country-year observation, which is why I will include them in a robustness test later on.

climate_conflict <- climate_conflict[!(climate_conflict$countryname == "Angola" & climate_conflict$years %in% 2000:2006),]
climate_conflict <- climate_conflict[!(climate_conflict$countryname == "Namibia" & climate_conflict$years %in% 1981:1990),]

## assigning NA to gdp in djibouti (all years) and liberia (1992 - 2002)
# ANALYTICAL CHOICE MADE OF TYPE OTHERS. FIRST RECORDED HERE.
# reason unclear !

climate_conflict[climate_conflict$years >= 1992 & climate_conflict$countryname == "Liberia", ]$gdp <- NA
climate_conflict[climate_conflict$countryname == "Djibouti",]$gdp <- NA

## assinging 0 instead of NA to conflict_onset var. in Congo, Dem. Rep, 1998:2000
# ANALYTICAL CHOICE MADE OF TYPE OTHERS. FIRST RECORDED HERE.

climate_conflict[climate_conflict$years %in% 1998:2000 & climate_conflict$countryname == "Congo, Dem. Rep.",]$conflict_onset <- NA

## check completeness óf data

view(climate_conflict[rowSums(is.na(climate_conflict)) >0 & climate_conflict$years <= 2002, ])


### regressions

climate_conflict$years <- as.numeric(climate_conflict$years) #need the numeric value of years for interaction term

#create table 1
table1_model1 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                    data = climate_conflict)
table1_model2 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                    data = climate_conflict)
table1_model3 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + gdp + polity2 +factor(iso3)*years,
                    data = climate_conflict)

#create table S1

tableS1_model1 <- lm(conflict ~ tmp + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model2 <- lm(conflict ~ tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model3 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model4 <- lm(conflict ~ pre + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model5 <- lm(conflict ~ pre_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model6 <- lm(conflict ~ pre + pre_lag + factor(iso3)*years,
                    data = climate_conflict)

tableS1_model7 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model8 <- lm(residuals(tableS1_model6) ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)


#create table S2

tableS2_model1 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS2_model2 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3) + years,
                     data = climate_conflict)

tableS2_model3 <- lm(conflict ~ tmp_diff + tmp_diff_lag + pre_diff + pre_diff_lag + factor(iso3)*years, 
                     data = climate_conflict)

tableS2_model4 <- lm(conflict ~ tmp_diff + tmp_diff_lag + pre_diff + pre_diff_lag + factor(iso3) + years,
                     data = climate_conflict)

#create table S4

tableS4_model1 <- lm(conflict_onset ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)
tableS4_model2 <- lm(conflict_onset ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)
tableS4_model3 <- lm(conflict_onset ~ tmp_diff + tmp_diff_lag + factor(iso3)*years,
                     data = climate_conflict)
tableS4_model4 <- lm(conflict_onset ~ tmp_diff + tmp_diff_lag + pre_diff + pre_diff_lag + factor(iso3)*years,
                     data = climate_conflict)

#create table S5 

tableS5_model1 <- lm(conflict ~ tmp + tmp_lag + tmp_lead + factor(iso3)*years,
                     data = climate_conflict)
tableS5_model2 <- lm(conflict ~ tmp + tmp_lag + tmp_lead + pre + pre_lag + pre_lead + factor(iso3)*years,
                     data = climate_conflict)

#create table S6

tableS6_model1 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS6_model2 <- lm(conflict ~ tmp + tmp_lag + gdp + factor(iso3)*years,
                     data = climate_conflict)

tableS6_model3 <- lm(conflict ~ tmp + tmp_lag + polity2 + factor(iso3)*years,
                     data = climate_conflict)

tableS6_model4 <- lm(conflict ~ tmp + tmp_lag + gdp + polity2 + factor(iso3)*years,
                     data = climate_conflict)

#create table S8

tableS8_model1 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)
tableS8_model2 <- lm(conflict ~ tmp + tmp_square + tmp_lag + tmp_lag_square + factor(iso3)*years,
                     data = climate_conflict)
tableS8_model3 <- lm(conflict ~ tmp + tmp_square + factor(iso3)*years,
                     data = climate_conflict)
tableS8_model4 <- lm(conflict ~ tmp + tmp_square + tmp_lag + tmp_lag_square + pre + pre_square + pre_lag + pre_lag_square + factor(iso3)*years,
                     data = climate_conflict)
