## we're comparing my results to the original files
## country observations
library(dplyr)
library(compare)
library(tidyverse)

rm(list = ls())

setwd("C:/R/bachelorproject/climate_conflict_replication_(original)")
climconf <- read_csv("./clim_conflict_for_R.csv")

unique(climconf$countryisocode)

iso3afralternative <- c("DZA","AGO","BEN","BWA","BFA","BDI","CMR","CPV","CAF","COM","COD","DJI","EGY",
                        "GNQ","ERI","ETH","GAB","GMB","GHA","GIN","GNB","CIV","KEN","LSO","LBR","LBY",
                        "MDG","MWI","MLI","MRT","MUS","MAR","MOZ","NAM","NER","NGA","COG","RWA","SHN",
                        "STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN","SWZ","TZA","TGO","TUN","UGA",
                        "ZMB","ZWE")
str(iso3afraltive)
iso3afrburke <- as.character(unique(climconf$countryisocode))
countrynamesafrburke <- as.character(unique(climconf$country))
countrynamesafrburke

iso3afralternative[!iso3afralternative %in% iso3afrburke]
iso3afrburke[!iso3afrburke %in% iso3afralternative]


##
rm(list=ls())
library(foreign)

setwd("C:/R/bachelorproject/")
pre <- read.csv("./csv_files/country_pre_ann.csv",stringsAsFactors = FALSE)
tmp <- read.csv("./csv_files/country_tmp_ann.csv",stringsAsFactors = FALSE)
original <- read.csv("./climate_conflict_replication_(original)/climate_conflict.dta",
                     stringsAsFactors = FALSE)

original_tmppre <- subset(original, select=c(countryisocode, year_actual, temp_all, prec_all ))
original_tmppre <- original_tmppre %>% rename(iso3 = countryisocode, years = year_actual)

## finding rows that are in the original replication files, but not my tables

anti_join(original_tmppre, pre, by=c("iso3","years"))

#the results is : data for period 2003-2006 for multiple countries --> that's ok for now
# + TCD and ZAR data for whole period -> not ok! why ?

#answer : ZAR is democratic republic of Congo , former ZAIRE --> in my data COD
#TCD Chad -> what's substitute ? let's check other way around

anti_join(pre, original_tmppre, by=c("iso3","years"))
#ok this get's messy

# whole period:
# COD : democratic republic of congo -> is ZAR in other sample
# COM: comoros
# dza: algeria
# egy: egypt
# eri: eritrea
# esh: western sahara -> disputed
# GNQ: equatorial giunea
# LBY: libya
# MAR: morocco
# MUS: mauritius
# NAM: namibia
# REU: Reunion -> part of France
# SSD: republic of south sudan
# TUN: tunisia
# 
#only part period: AGO

# change ZAIRE to COD in original data

original_tmppre$iso3[original_tmppre$iso3 =="ZAR"] <- "COD"

# possible reasons for all other isos: no conflict data ? 

#let's import some conflict data

###
rm(list=ls())

library(foreign)
setwd("C:/R/bachelorproject")
climate_conflict <- read_csv("./csv_files/climate_conflict.csv")
climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

yearsvector <- c(1981:2002)
years <- climate_conflict_original %>% filter(year_actual %in% yearsvector)

unique(years$year_actual) #22 unique
unique(years$countryisocode) #41 unique

# --> why only 889 obs? 

years <- years %>% select(c("year_actual", "countryisocode", "temp_all", "prec_all", "war_prio_new"))
years <- years %>% rename("years" = "year_actual", "iso3" = "countryisocode")
anti_join(climate_conflict, years)

#rename ZAR to COD, then only 13 obs. missing in original replication file (why?)

years$iso3[years$iso3 == "ZAR"] <- "COD" 

anti_join(climate_conflict, years) #better

##join tables

clim_conf_compare <- full_join(years, climate_conflict)

##compare conflict, tmp and pre 

#conflict
clim_conf_compare <- clim_conf_compare %>% mutate(conflict_diff = case_when(war_prio_new == conflict ~ 0, TRUE ~ 1))
                  
table(clim_conf_compare$conflict_diff) # we have 761 equal values and 141 where conflict is reported differently

#let's see where is more conflict reported

clim_conf_compare <- clim_conf_compare %>% mutate(conflict_diff = case_when(war_prio_new == conflict ~ 0,
                                                                            war_prio_new > conflict ~ 1,
                                                                            war_prio_new < conflict ~ -1))

table(clim_conf_compare$conflict_diff) # -1 = 128 , 0 = 761, 1 = 0

# --> only our conflict variable is overreported compared to origianl replication set !
# let's see where

conflict_diff_table <- clim_conf_compare %>% filter(conflict != war_prio_new) %>% select(years, iso3, war_prio_new, countryname, conflict, conflict_diff)

view(conflict_diff_table)

#let's research these in PRIO conflict table
## --> coding of this variable in the original replication file does not make any sense to me!!

### now compare tmp and pre

clim_conf_compare <- clim_conf_compare %>% mutate(tmp_diff = temp_all - tmp,
                                                  pre_diff = prec_all - pre)

clim_diff_table <- clim_conf_compare %>% select(years, iso3, countryname, temp_all,
                                                prec_all, tmp, pre, tmp_diff, pre_diff)

view(clim_diff_table)

#alright , this _diff columns don't look very zero-i
#relation?

plot(clim_diff_table$temp_all, clim_diff_table$tmp)
plot(clim_diff_table$temp_all, clim_diff_table$tmp_diff)
summary(lm(tmp_diff ~ temp_all, clim_diff_table))
#this is not good, the temperature value is statistically significant
#correlated with the temperaturedifference between our models

plot(clim_diff_table$prec_all, clim_diff_table$pre)
plot(clim_diff_table$prec_all, clim_diff_table$pre_diff)
summary(lm(pre_diff ~ prec_all, clim_diff_table))
#same

##well, it does make sense actually for higher values to have 
##bigger difference : dev. from absolut value would make a much better comparison

clim_diff_table <- clim_diff_table %>% mutate(tmp_diff_dev = tmp_diff/temp_all,
                                              pre_diff_dev = pre_diff/prec_all)

plot(clim_diff_table$temp_all, clim_diff_table$tmp_diff_dev)
summary(lm(tmp_diff_dev ~ temp_all, clim_diff_table))
#the effect is still statistically significant, but very marginal in quantitative perspective

plot(clim_diff_table$prec_all, clim_diff_table$pre_diff_dev)
summary(lm(pre_diff_dev ~ prec_all, clim_diff_table))
#for effect : same 
#but: there is a non-marginal intercept which is statistically significant!!
#which means, my values for precipitations are systematically lower (0.17% on mean) compared to burke data
