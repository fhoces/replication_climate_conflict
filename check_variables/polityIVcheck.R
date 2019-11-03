rm(list = ls())

setwd("C:/R/bachelorproject")

library(readxl)
library(foreign)
library(tidyverse)
library(compare)

#read in polityIV sets

polityIV2006 <- read_xls("./data/polity/p4v2006.xls")
polityIV2007 <- read_xls("./data/polity/p4v2007.xls")
polityIV2008 <- read_xls("./data/polity/p4v2008.xls")
polityIV2018 <- read_xls("./data/polity/polityIV.xls")

#read in original replication file

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

#check for countrynames

unique(climate_conflict_original$country[!climate_conflict_original$country %in% polityIV2006$country]) #are in the replication file but not in polity set
#Gambia, The 
#Cote d'Ivoire , 
#Congo , Repbulic of 
#Congo, Dem . Rep.

unique(polityIV2006$country[!polityIV2006$country %in% climate_conflict_original$country]) #other way around
#related countries are
#Gambia
#Ivory Coast
#Congo Brazzaville
#Congo Kinshasa

#we change it in original set because then we only have to do it once for all polity datasets

climate_conflict_original$country[climate_conflict_original$country == "Gambia, The"] <- "Gambia"
climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Ivory Coast"
climate_conflict_original$country[climate_conflict_original$country == "Congo, Republic of"] <- "Congo Brazzaville"
climate_conflict_original$country[climate_conflict_original$country == "Congo, Dem. Rep."] <- "Congo Kinshasa"

#check again
unique(climate_conflict_original$country[!climate_conflict_original$country %in% polityIV2006$country]) #super

#check others also
unique(climate_conflict_original$country[!climate_conflict_original$country %in% polityIV2007$country]) #super
unique(climate_conflict_original$country[!climate_conflict_original$country %in% polityIV2008$country]) #super
unique(climate_conflict_original$country[!climate_conflict_original$country %in% polityIV2018$country]) #super

#join tables

ccosub <- climate_conflict_original %>% select(country, year_actual, polity2)
pol06sub <- polityIV2006 %>% select(country, year,"polity2_06" = polity2 )
pol07sub <- polityIV2007 %>% select(country, year,"polity2_07" = polity2 )
pol08sub <- polityIV2008 %>% select(country, year,"polity2_08" = polity2 )
pol18sub <- polityIV2018 %>% select(country, year,"polity2_18" = polity2 )


polity_check <- list(ccosub, pol06sub, pol07sub, pol08sub, pol18sub) %>% reduce(left_join, by = c("year_actual" = "year", "country"))

#15 more obs.

table(polity_check$country) - table(climate_conflict_original$country) # Ethiopia has 15 extra entries

polity_check[polity_check$country == "Ethiopia",] # multiple entries for 1993
#looking at data shows we have multiple entries for all the datasets, combination makes the multiple entries . let's just look at the combis

#let's just compare

polity_diff <- polity_check %>% mutate(diff06 = polity2 - polity2_06,
                                       diff07 = polity2 - polity2_07,
                                       diff08 = polity2 - polity2_08,
                                       diff18 = polity2 - polity2_18)

table(polity_diff$diff06)
table(polity_diff$diff07)
table(polity_diff$diff08)
table(polity_diff$diff18)

#maybe NA's 

polity_diffnona <- polity_diff[complete.cases(polity_diff),]

# no

polity_diff[!polity_diff$diff18 == 0,]

#they must have used 07, the 8 diff comes from the multiple ethiopia obs.

## let's test for only 07

rm(list = ls())

#import

polityIV2007 <- read_xls("./data/polity/p4v2007.xls")

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

#change countrynames

climate_conflict_original$country[climate_conflict_original$country == "Gambia, The"] <- "Gambia"
climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Ivory Coast"
climate_conflict_original$country[climate_conflict_original$country == "Congo, Republic of"] <- "Congo Brazzaville"
climate_conflict_original$country[climate_conflict_original$country == "Congo, Dem. Rep."] <- "Congo Kinshasa"

#subset

ccosub <- climate_conflict_original %>% select(country, year_actual, polity2)

pol07sub <- polityIV2007 %>% select(country, year,"polity2_07" = polity2 )

polity_check07 <- left_join(ccosub, pol07sub, by = c("country", "year_actual" = "year"))

polity_check07 <- polity_check07 %>% mutate(pol_diff = polity2_07 - polity2)

table(polity_check07$pol_diff) # 1 difference

polity_check07[which(polity_check07$pol_diff == 1),]
view(polity_check07)

# this is the double entry for ethiopia. they decided to use the value 0 (for whatever reasons)

# we are going to use the polity IV dataset version 2007 , and vlaue 0  for 1993 ethiopia too in the replication then

#done.

##control with my data 

rm(list = ls())

setwd("C:/R/bachelorproject")
#my data

my_climate_conflict <- read_csv("./csv_files/climate_conflict.csv") 

my_climate_conflict <- my_climate_conflict %>% select(years, countryname, polity2)

#burke data
climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

ccosub <- climate_conflict_original %>% select(year_actual, country, polity_cco =  polity2)

table(my_climate_conflict$years)
table(ccosub$year_actual)
#40 vs 41 in mine

table(ccosub$country)
table(my_climate_conflict$countryname)
#in original, instead of 26 for most , in angola 19 and in namibia 16

unique(my_climate_conflict$countryname[!my_climate_conflict$countryname %in% ccosub$country])

unique(ccosub$country[!ccosub$country %in% my_climate_conflict$countryname])

ccosub$country[ccosub$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

unique(my_climate_conflict$countryname[!my_climate_conflict$countryname %in% ccosub$country])

mydata_ccosub <- my_climate_conflict %>% right_join(ccosub, by = c("years" = "year_actual", "countryname" = "country"))

compare(mydata_ccosub$polity2, mydata_ccosub$polity_cco) # TRUE -> for all country year obs. same value

