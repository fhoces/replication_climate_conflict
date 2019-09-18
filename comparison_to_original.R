##############################################################################################
rm(list=ls())

library(foreign)
library(dplyr)
library(compare)
library(tidyverse)

setwd("C:/R/bachelorproject")
climate_conflict <- read_csv("./csv_files/climate_conflict.csv")
climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

yearsvector <- c(1981:2002)
climate_conflict_original <- climate_conflict_original %>% filter(year_actual %in% yearsvector)

uniqueN(climate_conflict_original$year_actual) #22 unique
uniqueN(climate_conflict_original$countryisocode) #41 unique

# --> why only 889 obs? 

climate_conflict_original <- climate_conflict_original %>% select(c("year_actual", "countryisocode", "temp_all", "prec_all", "war_prio_new", "gdp", "polity2"))
climate_conflict_original <- climate_conflict_original %>% rename("years" = "year_actual", "iso3" = "countryisocode")
anti_join(climate_conflict, climate_conflict_original, by = c("iso3", "years"))

#rename ZAR to COD, then only 13 obs. missing in original replication file (why?)

climate_conflict_original$iso3[climate_conflict_original$iso3 == "ZAR"] <- "COD" 

anti_join(climate_conflict, climate_conflict_original, by = c("iso3", "years")) #better , only namibia and 3 years of angola missing

##join tables

clim_conf_compare <- full_join(climate_conflict_original, climate_conflict, by = c("years", "iso3"))

##compare conflict, tmp and pre 

#conflict
clim_conf_compare <- clim_conf_compare %>% mutate(conflict_diff = case_when(war_prio_new == conflict ~ 0, TRUE ~ 1))
                  
table(clim_conf_compare$conflict_diff) # we have 761 equal values and 141 where conflict is reported differently

length(clim_conf_compare$war_prio_new[is.na(clim_conf_compare$war_prio_new)]) # 13 missing 
length(clim_conf_compare$conflict[is.na(clim_conf_compare$conflict)])

#let's see where is more conflict reported

clim_conf_compare <- clim_conf_compare %>% mutate(conflict_diff = case_when(war_prio_new == conflict ~ 0,
                                                                            war_prio_new > conflict ~ 1,
                                                                            war_prio_new < conflict ~ -1))

table(clim_conf_compare$conflict_diff) # -1 = 128 , 0 = 761, 1 = 0

# --> only our conflict variable is overreported compared to original replication set !
# let's see where..

conflict_diff_table <- clim_conf_compare %>% 
  filter(conflict != war_prio_new) %>% select(years, iso3, war_prio_new, countryname, conflict, conflict_diff)

view(conflict_diff_table)

#let's research these in PRIO conflict table
## --> coding of this variable in the original replication file does not make any sense to me!!
## 
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
