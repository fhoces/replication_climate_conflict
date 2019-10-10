# script parts for future use 

#pwt 9.1
data("pwt9.1")
view(pwt9.1)
pwt9.1$isocode <- as.character(pwt9.1$isocode)
pwt9.1$year <- as.character(pwt9.1$year)

#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. FIRST RECORDED HERE.
#defining GDP as real GDP at constant national prices
pwt9.1 <- pwt9.1 %>% filter(year>= 1981, year<= 2006) %>% select(iso3 = isocode, years = year, GDP_pwt9.1 = rgdpna, pop_pwt9.1 = pop)

pwt9.1 <- pwt9.1 %>% mutate(GDPpc_pwt9.1 = GDP_pwt9.1/pop_pwt9.1) #GDP per capita (PPP) in 2011$

pwt9.1 <- pwt9.1 %>% select(iso3, years, GDPpc_pwt9.1)

climate_conflict <- left_join(climate_conflict, pwt9.1, by = c("iso3", "years"))

table(is.na(climate_conflict$GDPpc_pwt9.1)) #26 missing values

view(climate_conflict[is.na(climate_conflict$GDPpc_pwt9.1),]) #SOM

##Polity IV data 

polity_url <- "http://www.systemicpeace.org/inscr/p4v2018.xls"
dest_polity <- "./data/polity/polityIV.xls"

if(!file.exists(dest_polity)) {
  
  download.file(polity_url, dest_polity, mode = "wb")
}


polity <- read_xls(dest_polity)
view(polity)

##scodes are different from iso3 codes, so we have to redine them
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

climate_conflict <- left_join(climate_conflict, polityjoin, by = c("iso3", "years")) ## ui, we have one more obs. now .. what happened there?
table(climate_conflict$countryname) #ethiopia has 23 obs. instead of 22

#looking that up in the polityIV table shows they apparently changed the countrycode in 1993 and have two observations for that year
#quick wikipedia search shows they got a new constitution in 1994, probably has something to do with that
#it's behind my scope to decide which one is better.. but it changed only marginaly from 0 to 1 
#i would drop one randomly , but not so good for reproducability and transparency reasons
#so I will just drop the worse one (with polity2 score being 0) (the effect of this on the analysis will be assumed to be close to not existent)
#ANALYTICAL CHOICE OF TYPE DATA-SUBSETTING. FIRST RECORDED HERE.

climate_conflict <- climate_conflict[!(climate_conflict$countryname == "Ethiopia" & climate_conflict$years == 1993 & climate_conflict$polity2 == 0),]



table(is.na(climate_conflict$polity2)) #9 missing values
polityNA <- climate_conflict[is.na(climate_conflict$polity2),]
view(polityNA) #Namibia politic score only starts in 1990


#GDP from world bank
str(wb_cachelist, max.level = 1)

income_vars <- wbsearch(pattern = "gross domestic")
view(income_vars)

#ANALYTICAL CHOICE OF TYPE VARIABLE DEFINITION. RECORDED FIRST HERE. 
#I download GDP per capita information from the World Bank Atlas. 
#GDP is used by Burke as well, but in the old version of the WDI (May 2009), in 1985 USD values
#this old data is not possible to download through wbstats package yet --> email to Jesse sent! 
#maybe download manually and see if there is a difference 

world_bank <- wb(indicator = c("NY.GDP.PCAP.CD","NY.GDP.PCAP.PP.CD"), startdate = 1981, enddate = 2006, return_wide = T)

world_bank_GDPpc_currentUSD <- as.data.frame(world_bank %>% select(iso3 = iso3c, years = date, GDP_WB = NY.GDP.PCAP.CD))

world_bank_GDPpcPPP <- as.data.frame(world_bank %>% select(iso3 = iso3c, years = date, GDPpp_WB = NY.GDP.PCAP.PP.CD))

climate_conflict <- left_join(climate_conflict, world_bank_GDPpc_currentUSD, by = c("iso3", "years"))
climate_conflict <- left_join(climate_conflict, world_bank_GDPpcPPP, by = c("iso3", "years"))
view(climate_conflict)

view(climate_conflict[is.na(climate_conflict$GDP_WB) & climate_conflict$years <= 2002,]) #different ones
view(climate_conflict[is.na(climate_conflict$GDPpp_WB) & climate_conflict$years <= 2002,]) #alot
table(climate_conflict$countryname[is.na(climate_conflict$GDPpp_WB) & climate_conflict$years <= 2002]) #djibouti & somalia for all years


plot(climate_conflict$GDP_WB, climate_conflict$GDPpp_WB)


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
