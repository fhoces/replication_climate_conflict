## we're comparing my esults to the original files

rm(list=ls())

library(compare)
library(dplyr)

setwd("C:/R/bachelorproject/")
pre <- read.csv("./csv_files/country_pre_ann.csv",stringsAsFactors = FALSE)
tmp <- read.csv("./csv_files/country_tmp_ann.csv",stringsAsFactors = FALSE)
original <- read.csv("C:/Users/marcr/Downloads/climate_conflict_replication_(original)/climate_conflict_replication (original)/clim_conflict_for_R.csv", stringsAsFactors = FALSE)

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


