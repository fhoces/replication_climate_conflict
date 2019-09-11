##set up

rm(list = ls())

setwd("C:/R/bachelorproject")

library(ncdf4)
library(tidyverse)
library(chron)
library(rgdal)
library(readxl)
library(splitstackshape)

## importing the CRU data v4.03 (all)

#set path and file name

ncpath <- "C:/R/bachelorproject/data/cru_data/"
ncname <- "cru_ts4.03.1901.2018.tmp.dat"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "tmp"

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
names(tmp_all_df)[3:ncol(tmp_all_df)] <- paste(rep(years, each=12), rep(month, times=118))

str(tmp_all_df)

#drop irrelevant columns , leaving only 1981-2002

years1 <- 1981:2002

removecols1 <- c("lon","lat")
removecols2 <- paste(rep(years1, each=12), rep(month, times=22))
removecols <- c(removecols1,removecols2)
head(removecols,13)
tail(removecols,13)
head(tmp_all_df)

tmp_red_df <- tmp_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the tmp at the time we need: 1981-2002
head(tmp_red_df)
head(na.omit(tmp_red_df))

rm(tmp_all_df)


##import the country geodata through the GADM shapefile
#ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. RECORDED FIRST HERE. I USE THE GADM DATASET AS A SOURCE FOR COUNTRY GRID INFORMATION. 
#UNFORTUNATELY AUTHORS DIDN'T DEFINE, WHICH SOURCE THEY ARE USING.

#import on country level

gadmshape0 <- readOGR(dsn = "./data/gadm/gadm36_0.shp", layer = "gadm36_0")
class(gadmshape0)
summary(gadmshape0)
gadmshape0$NAME_0
gadmshape0$GID_0

#catch dataframe of iso codes and name
countrypolygons.df <- as.data.frame(gadmshape0)
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
row.names(tmp_redafr_df) <- c(1:17375)

#turn tmp_red_df into Spatialpoints

tmp_coords <- cbind(tmp_redafr_df$lon, tmp_redafr_df$lat)
tmp_pts <- SpatialPoints(coords = tmp_coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
tmp_pts

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

  
## the temperature is first averaged over the grid cells in a country, then over the month of a year
# ANALYTICAL CHOICE OF TYPE PROCESSING - OTHERS. FIRST RECORDED HERE.

#averaging over cells in country

country_tmp <- aggregate(full_tmp[2:265], list(full_tmp$GID_0), mean)
names(country_tmp)[1] <- "iso3"
country_tmp

# restructuring the table -> we want the tmp information for the month not in columns but in one column , each obs. being a row

country_tmp_num <- subset(country_tmp, select = -iso3) #need this subset with only the numerical values to use function ; _num stands for numerical
tmp_vec1 <- as.vector(t(country_tmp_num))
length(tmp_vec1)
iso3 <- subset (country_tmp, select = iso3)
iso3 <- as.vector(t(iso3))
iso3rep <- unlist(rep(iso3,each = 264))  #countrycodes for all the 264 obs. of tmp per country (12m*22y)
summary(iso3rep)
iso3rep
colnames1 <- colnames(country_tmp_num)

country_tmp <- data.frame(iso3rep,colnames1,tmp_vec1) 
colnames(country_tmp) <- c("iso3","months", "tmp") 
summary(country_tmp)
dim(country_tmp)
# calculate yearly data

country_tmp_ann <- country_tmp %>% separate(months, into=c("years", "months")) %>% group_by(iso3, years)%>% summarise(tmp=mean(tmp))
dim(country_tmp_ann)

### temperature finished

write_csv(country_tmp_ann, "C:/R/bachelorproject/csv_files/country_tmp_ann.csv")

##importing precipitation

ncpath <- "C:/R/bachelorproject/data/cru_data/"
ncname <- "cru_ts4.03.1901.2018.pre.dat"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "pre"

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
names(pre_all_df)[3:ncol(pre_all_df)] <- paste(rep(years, each=12), rep(month, times=118))

str(pre_all_df)

#drop irrelevant columns , leaving only 1981-2002

pre_red_df <- pre_all_df %>% select(!!removecols) #_red_ stands for reduced, we now have a dataframe of the tmp at the time we need: 1981-2002
head(tmp_red_df)
head(na.omit(tmp_red_df))

rm(pre_all_df)

##get pre data for the single countries

#delete irrelevant grids in the pre_red_df dataframe
#we see in gadmshape0afr for which extent we need data for

pre_redafr_df <- subset(pre_red_df, lon >= -17.75 & lon <=51.5 & lat >= -35 & lat <= 27.5)
head(pre_redafr_df)

#rename the rows
row.names(pre_redafr_df) <- c(1:17375)

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

country_pre <- aggregate(full_pre[2:265], list(full_pre$GID_0), mean)
names(country_pre)[1] <- "iso3"
country_pre

# restructuring the table -> we want the pre information for the month not in columns but in one column , each obs. being a row

country_pre_num <- subset(country_pre, select = -iso3) #need this subset with only the numerical values to use function ; _num stands for numerical
pre_vec1 <- as.vector(t(country_pre_num))
length(pre_vec1)

country_pre <- data.frame(iso3rep,colnames1,pre_vec1) 
colnames(country_pre) <- c("iso3","months", "pre") 

# calculate yearly data

country_pre_ann <- country_pre %>% separate(months, into=c("years", "months")) %>% group_by(iso3, years) %>% summarise(mean=mean(pre))

dim(country_pre_ann)

#write into file

write_csv(country_pre_ann, "C:/R/bachelorproject/csv_files/country_pre_ann.csv")


### precipitation finished

### import conflict data

conflict <- read_xls("./data/conflict/MainConflictTable.xls")

## filter down to relevant data

#delete obs. not between 1981 and 2002

conflict <- conflict %>% filter(YEAR >= 1981 & YEAR <=2002)

conflict
#observations in african countries

table(conflict$Location)

#duplicate the rows which have multiple locations

conflict <- cSplit(conflict, "Location", sep = ",", direction = "long")

#rename and define the incidence of conflict as conflict in the countries territory -> this seems to be what burke et al. did

#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. RECORDED FIRST HERE.

conflict <- conflict %>% rename(countryname = Location)

#get countrynames from burke

##ANALYTICAL CHOICE OF TYPE DATA SUB-SETTING. RECORDED FOR FIRST TIME IN LINE 144.

africancountries <- data.frame(iso3afr,
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
                                               "Sudan")) #these are from the original replication analytic data
                                                                                                                                                    
              
#let's see which countries are in conflict table that we didn't define

unique(conflict$countryname[!conflict$countryname %in% africancountries$countryname]) #are in PRIO but not in our african countryset

#the result are these (narrowed to african countries) , which might have been just different spelled or defined in burke dataset: 
#c( "Yemen (Arab Republic of Yemen)", "Democratic Republic of Congo (Zaire)", "Morocco", "Mozambique", "Gambia", 
#   "Democratic Republic of Yemen", "People's Republic of Yemen", "Lybia", "Comoros", "Algeria", "Egypt", "Congo", "Cote D'Ivoire") 
length(renamecountries)

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
##ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. RECORDED FIRST IN LINE 419.
#if changing the variable defintion for conflict, e.g. not being location but SideA, then needs to be changed here too.
conflict <- conflict %>% 
  filter(countryname %in% africancountries$countryname) %>% 
  select(countryname, YEAR)

#create iso3 in conflict table for merger
  
conflict <- left_join(conflict, africancountries)

###import of conflict finished for now

###merge tmp, pre and conflict

full_join(country_tmp_ann, country_pre_ann) %>%
  full join(., conflict)

