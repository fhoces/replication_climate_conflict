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


view(cru_check[rowSums(is.na(cru_check)) >0 & cru_check$years <= 2002, ]) #no missing values both sides for 1981 - 2002

ggplot(cru_check, aes(tmp, temp_all)) +
  geom_point(aes(colour = factor(countryname)))

ggplot(cru_check, aes(pre, prec_all)) +
  geom_point(aes(colour = factor(countryname)))

ggplot(cru_check, aes())
marrangeGrob(plot_lst, nrow = 2, ncol = 2)
