## we're comparing my esults to the original files

rm(list=ls())

library(compare)
library(dplyr)

setwd("C:/R/bachelorproject/")
pre <- read.csv("./csv_files/country_pre_ann.csv")
tmp <- read.csv("./csv_files/country_tmp_ann.csv")
original <- read.csv("C:/Users/marcr/Downloads/climate_conflict_replication_(original)/climate_conflict_replication (original)/clim_conflict_for_R.csv")

original_tmppre <- subset(original, select=c(countryisocode, year_actual, temp_all, prec_all ))
original_tmppre <- original_tmppre %>% rename(iso3 = countryisocode, years = year_actual)

isotimepre <- subset(pre, select = c(iso3, years))
isotimetmp <- subset(tmp, select = c(iso3, years))
compare(isotimepre, isotimetmp, allowAll = TRUE)

isotimeoriginial <- subset(original_tmppre, select = c(countryisocode, year_actual))

anti_join(original_tmppre, pre, by=c("iso3","years"))
