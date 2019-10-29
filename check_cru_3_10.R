# checking CRU 3_10

##set up

rm(list = ls())

setwd("C:/R/bachelorproject")

#load packages
package_load <- function(x) {
  for (i in x) {
    # require returns TRUE invisibly if it was able to load packages
    if ( ! require (i, character.only = TRUE)) {
      # IF package was not able to be loaded ten re-install
      install.packages(i, dependencies = T)
      # Load package after installing
      require( i, character.only = T)
    }
    
  }
}

packages <- c("ncdf4","tidyverse", "chron", "rgdal", "readxl", "splitstackshape", "purrr", "fastDummies",
              "wbstats", "pwt","pwt9", "data.table", "foreign", "plm", "stargazer", "R.utils")

package_load(packages)

# import orginal + filter

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

climate_conflict_original <- climate_conflict_original %>% select(country, year_actual, 
                                                                  temp_all,temp_all_lag, temp_all_dif, temp_all_diflag, 
                                                                  prec_all,prec_all_lag, prec_all_dif, prec_all_diflag, 
                                                                  gpcp, gpcp_l, gpcp_d, gpcp_d_l)

# import 4 calc

mydata4 <- read_csv("./csv_files/climate_conflict.csv")
mydata4 <- mydata4 %>% select(countryname, years,
                            tmp, tmp_lag, tmp_lead, tmp_square,tmp_lag_square,
                            tmp_diff, tmp_diff_lag, tmp_difftrend,
                            pre, pre_lag, pre_lead, pre_square, pre_lag_square,
                            pre_diff, pre_diff_lag, pre_difftrend,
                            gpcp)

# import 3 calc

mydata3 <- read_csv("./csv_files/climate_conflict_3_10.csv")
mydata3 <- mydata3 %>% select(countryname, years,
                             tmp, tmp_lag, tmp_lead, tmp_square,tmp_lag_square,
                             tmp_diff, tmp_diff_lag, tmp_difftrend,
                             pre, pre_lag, pre_lead, pre_square, pre_lag_square,
                             pre_diff, pre_diff_lag, pre_difftrend,
                             gpcp)


climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

## compare the original data with my computation of CRU 3.10
climate_check <- climate_conflict_original %>% left_join(mydata3, by = c("year_actual" = "years" , "country" = "countryname"))


view(climate_check[rowSums(is.na(climate_check)) > 0 & climate_check$years <= 2002, ]) #no missing values both sides for 1981 - 2002

## check tmp and pre

ggplot(climate_check, aes(tmp, temp_all)) +
  geom_point(aes(colour = factor(country)))

ggplot(climate_check, aes(pre, prec_all)) +
  geom_point(aes(colour = factor(country)))

## compare my compuation of CRU 3.10 with my computation of CRU 4.03

climate_check <- mydata3 %>% left_join(mydata4, by = c("years", "countryname"))
view(climate_check[rowSums(is.na(climate_check)) > 0 & climate_check$years <= 2002, ]) #no missing values both sides for 1981 - 2002

## check tmp and pre

ggplot(climate_check, aes(tmp.x, tmp.y)) +
  geom_point(aes(colour = factor(countryname)))

ggplot(climate_check, aes(pre.x, pre.y)) +
  geom_point(aes(colour = factor(countryname)))
