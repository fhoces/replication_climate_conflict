## we're comparing my esults to the original files

rm(list=ls())

setwd("C:/R/bachelorproject/")
pre <- read.csv("./csv_files/country_pre_ann.csv")
tmp <- read.csv("./csv_files/country_tmp_ann.csv")
original <- read.csv("C:/Users/marcr/Downloads/climate_conflict_replication_(original)/climate_conflict_replication (original)/clim_conflict_for_R.csv")
