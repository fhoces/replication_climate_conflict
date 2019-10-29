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


tmp <- readLines("./data/cru_data/cru_ts_2_10.1901-2002.tmp")
tmp <- tmp[6:length(tmp)]

gridrefs <- grep(tmp, pattern = "^Grid-ref", value=TRUE)
head(gridrefs)

gridtable <- read.table(text = gridrefs)
names(gridtable) <- c("grid-ref", "x", "y")
gridtable$x <- as.numeric(gridtable$x)
gridtable$y <- as.numeric(gridtable$y)


tmplines <- grep(x = tmp, pattern = "^Grid-ref", value = TRUE, invert = TRUE)
tmptable <- read.table(text = tmplines)
names(tmptable) <- c("Jan","Feb","Mar","Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec")


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

tmp_pts_count <- as.data.frame(tmp_pts)
tmp_pts_count <- rename(count(tmp_pts_count, coords.x1, coords.x2), Freq = n)
table(tmp_pts_count$Freq) #all spatial points sets have same frequency --> good!

#get the location for the grid cells in tmp dataframe

loc_tmps <- tmp_pts %over% gadmshape0afr
dim(na.omit(loc_tmps))
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

full_tmp <- full_tmp %>% dplyr::select(-NAME_0, -lon, -lat)

country_tmp <- aggregate(full_tmp[3:ncol(full_tmp)], by = c(list(full_tmp$GID_0), list(full_tmp$yearsrep)), mean)
names(country_tmp)[1] <- "iso3"
names(country_tmp)[2] <- "years"

country_tmp <- country_tmp %>% mutate(yearly_mean = rowMeans(dplyr::select(.,"Jan":"Dec"))) %>% dplyr::select(iso3, years, tmp = yearly_mean) %>% arrange(iso3, years)

country_tmp$tmp <- country_tmp$tmp*0.1 # for correct degree value

write_csv(country_tmp, "./csv_files/cru2_1_tmp.csv")

## the check (see check_cru2_10.R) shows that they don't match the original results.

## do same for precipitation



pre <- readLines("./data/cru_data/cru_ts_2_10.1901-2002.pre")
pre <- pre[6:length(pre)]

gridrefs <- grep(pre, pattern = "^Grid-ref", value=TRUE)
head(gridrefs)

gridtable <- read.table(text = gridrefs)
names(gridtable) <- c("grid-ref", "x", "y")
gridtable$x <- as.numeric(gridtable$x)
gridtable$y <- as.numeric(gridtable$y)


prelines <- grep(x = pre, pattern = "^Grid-ref", value = TRUE, invert = TRUE)

pretable <- read.table(text = prelines)
#lines corrupt, because too big values join together... correct:
#ANALYTICAL CHOICE MADE OF TYPE PROCESSING - OTHERS. 
#I assume the way which digits of the joined numbers are part of the numbers.

prelines[7108] <- as.character("2081 1538 2457 4273 1585 1802 1132 655 880 2780 1819 10557")
prelines[70280] <- as.character("1053 11961 4465 3663 3361 965 1409 3437 1189 2647 4849 5707")
prelines[70303] <- as.character("1613 2173 10326 3495 3176 2418 1294 3197 1120 2932 1811 3430")
prelines[70322] <- as.character("152 3452 10207 1966 3154 1632 472 1078 1190 1614 2217 1929")

#this is gonna take ages, let's check how many values

pretable <- read.table(text = prelines, fill = T)
dim(pretable[rowSums(is.na(pretable)) > 0 ,]) # there's 7672 lines where two obs. join together because they're more than 4 digits each

# hm.. somehow code who splits numbers with more than 4 digits ... 
# kind of in the concept of 
# for (i in prelines) {
#   strsplit(i, " ")
#   for (j in prelines[i]) {
#     if (length(j) > 4) { ??
#       
#     }
#   }
# }
