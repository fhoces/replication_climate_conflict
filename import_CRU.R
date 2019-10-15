rm(list=ls())

library(tidyverse)

setwd("C:/R/bachelorproject/")

tmp <- readLines("./cruimport/cru_ts_2_10.1901-2002.tmp")
tmp <- tmp[6:length(tmp)]

gridrefs <- grep(tmp, pattern = "^Grid-ref", value=TRUE)
head(gridrefs)

gridtable <- read.table(text = gridrefs)
names(gridtable) <- c("grid-ref", "x", "y")
gridtable$x <- as.numeric(gridtable$x)
gridtable$y <- as.numeric(gridtable$y)


tmplines <- grep(x = tmp, pattern = "^Grid-ref", value = TRUE, invert = TRUE)
tmptable <- read.table(text = tmplines)
names(tmptable) <- c("Jan","Feb","Mar","Apr", "Jun","Jul","Aug","Sep","Oct","Nov","Dec")


years <- c(1901:2002)
yearsrep <- rep(years, times = nrow(gridtable/length(years)))

lon <- rep(gridtable$x, each = length(years))
lat <- rep(gridtable$y, each = length(years))

gridtable <- cbind(lon, lat, yearsrep)

head(gridtable)
dim(gridtable)

tmp <- data.frame(gridtable, tmptable)

#drop irrelevant columns , leaving only 1979 - 2006
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED HERE.
#I include years 1979 - 1980 to compute climate lags and climate_diff lags. 
#From the original data, I can not see how they computed the lag variable for 1981, but I assume this is how they did it , too.

years1 <- 1979:2006

tmp <- tmp %>% filter(yearsrep >= 1979)

tmp$lon = tmp$lon/2 - 180 - 0.25
tmp$lat = tmp$lat/2 - 90 - 0.25

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

tmp_redafr <- subset(tmp, lon >= -17.75 & lon <=51.5 & lat >= -35 & lat <= 27.5)
head(tmp_redafr)

#rename the rows
row.names(tmp_redafr) <- c(1:nrow(tmp_redafr))

#turn tmp_red_df into Spatialpoints

tmp_coords <- cbind(tmp_redafr$lon, tmp_redafr$lat)
tmp_pts <- SpatialPoints(coords = tmp_coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
tmp_pts

#get the location for the grid cells in tmp dataframe

loc_tmps <- tmp_pts %over% gadmshape0afr
na.omit(loc_tmps)
summary(loc_tmps)

##merging the temperature (incl. lon +lat) with the country codes

str(loc_tmps)
str(tmp_redafr)
head(na.omit(loc_tmps))

full_tmp <- bind_cols(loc_tmps, tmp_redafr)
dim(full_tmp)

#delete obs. with either no tmp-data or not defined for an african country

full_tmp <- na.omit(full_tmp)
dim(full_tmp)
head(full_tmp)

#NAME_0 is unnecessary, and we won't need lon and lat anymore

full_tmp <- full_tmp %>% select(-NAME_0, -lon, -lat)

