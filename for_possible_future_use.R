

###let's do some first regressions.

##reproduction original files 

#version1
countrytimetrends <- grep("^Iccyear", names(climate_conflict_original), value = T) 
countryfe <- grep("^iccode", names(climate_conflict_original), value = T)
formula1 <- reformulate(termlabels = c("temp_all",
                                       "temp_all_lag",
                                       countrytimetrends,
                                       countryfe),
                        
                        response = "war_prio_new")
model2 <- lm(formula1, data = climate_conflict_original)

blabla2 <- summary(model2)
summary(model2, robust = T, cluster = c("ccode"))

##looking good

#version2

climate_conflict_original$years <- as.numeric(climate_conflict_original$years) #has to be numeric for regression
model3 <- lm(war_prio_new ~ temp_all + temp_all_lag + factor(iso3)*years,data = climate_conflict_original)
blabla3 <- summary(model3)
summary(model3, robust = T, cluster = c("ccode"))

#same results


##use our data

climate_conflict$years <- as.numeric(climate_conflict$years) #need again the numeric value for interaction term

modelme <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years, data = climate_conflict)
blablame <- summary(modelme)
## results differ a bit.. tmp higher and tmp_lag much higher

modelme1 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
               data = climate_conflict)
summary(modelme1)

modelme2 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
               data = climate_conflict)
summary(modelme2)
#my results are a bit higher than original ones, again

plot(modelme1)
stargazer(model2, model3, modelme1)


blabla2$coefficients[ 1:3,]
blabla3$coefficients[ 1:3,]
blablame$coefficients[1:3,]

view(climate_conflict)

##### import GPCP precipitation

# the method suggested below does not work . we have too little amount of grids - not sure why
# computing with the below method does result in only half countries receiving information,
# with the precipitation being completely different from original dataset (see climate_check.R)

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

 # convert time , strgsplit (not compulsory)

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
 dim(gpcp_all_df)
 rm(gpcp_array)
 rm(gpcp_vec)
 rm(gpcp_mat)

 #rename col name

 years <- 1979:2019
 names(gpcp_all_df)[1:2] <- paste(c("lon","lat"))
 names(gpcp_all_df)[3:ncol(gpcp_all_df)] <- paste(rep(years, each=12), rep(month, times = length(years)))

 colnames(gpcp_all_df)

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

 country_gpcp_ann$gpcp <- country_gpcp_ann$gpcp/100

 #write into file

 write_csv(country_gpcp_ann, "C:/R/bachelorproject/csv_files/country_gpcp_ann.csv")

## GPCP finished

