## check difference on CRU climate variables

rm(list = ls())

library(foreign)
library(tidyverse)
library(cowplot)
library("gridExtra")

setwd("C:/R/bachelorproject")

# import orginal + filter

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

climate_conflict_original <- climate_conflict_original %>% select(country, year_actual, temp_all,temp_all_lag, temp_all_dif, temp_all_diflag, prec_all,prec_all_lag, prec_all_dif, prec_all_diflag)

# import our data

cru_check <- read_csv("./csv_files/climate_conflict.csv")

unique(climate_conflict_original$country[!climate_conflict_original$country %in% cru_check$countryname]) # Cote d`Ivoire again

unique(cru_check$countryname[!cru_check$countryname %in% climate_conflict_original$country])


climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

cru_check <- cru_check %>% right_join(climate_conflict_original, by = c("years" = "year_actual", "countryname" = "country"))

## note that we right-joined ... we seize down to original data obserations, whereas in our data sample there's more observations.


view(cru_check[rowSums(is.na(cru_check)) > 0 & cru_check$years <= 2002, ]) #no missing values both sides for 1981 - 2002

ggplot(cru_check, aes(tmp, temp_all)) +
  geom_point(aes(colour = factor(countryname)))

ggplot(cru_check, aes(pre, prec_all)) +
  geom_point(aes(colour = factor(countryname)))

ggplot(cru_check, aes())
marrangeGrob(plot_lst, nrow = 2, ncol = 2)


## 

cru_check <- cru_check %>% mutate(tmp_temp_diff = tmp - temp_all,
                                  tmp_temp_dev = tmp_temp_diff/temp_all,
                                  pre_prec_diff = pre - prec_all,
                                  pre_prec_dev = pre_prec_diff/prec_all)


ggplot(cru_check, aes(temp_all, tmp_temp_dev)) +
  geom_point(aes(colour = factor(countryname)))
ggplot(cru_check, aes(years, tmp_temp_dev)) +
  geom_point(aes(colour = factor(countryname)))

#there is no pattern detectable for temperature !!

#confirm with regression 

summary(lm(tmp_temp_dev ~ temp_all + factor(countryname),
           data = cru_check))

# small influcence of temperature 
# note that the regressor for temp_all estimates the effect of 1 unit increase in temperature on deviation
# the difference between 1st qu. and 3rd qu. in our temperature observations is smaller than 5 units

ggplot(cru_check, aes(prec_all, pre_prec_dev)) +
  geom_point(aes(colour = factor(countryname)))
ggplot(cru_check, aes(years, pre_prec_dev)) +
  geom_point(aes(colour = factor(countryname)))

## there is a small pattern in precipitation : surprisingly , for smaller precipitation, there's a bigger deviation !
## from the plot a bit difficult to see , which parts country's play, let's check with regression

summary(lm(pre_prec_dev ~ prec_all + factor(countryname),
           data = cru_check))

# pre_all is higly significant for determining the deviation

# again , effect not huge -> regressor estimates , again, effect of 1 unit increase in temp.
# more than difference between 1st and 3rd quantile in  obs. of complete data

# check for tmp 

t.test(cru_check$tmp_temp_diff)
t.test(cru_check$pre_prec_diff)
t.test(cru_check$tmp_temp_dev)
t.test(cru_check$pre_prec_dev)
## we also see, that one country is really different

view(cru_check %>% filter(pre_prec_dev < -0.4))

## Sudan has (for all years) a deviation bigger than 0.4 (in absolute values !)
## this perhaps comes from a different calculation method, previously manipulated data... really , could be a lot of reasons! 
## just have to keep that in mind, together with overall smaller precipitation values in our data

summary(cru_check$prec_all)
