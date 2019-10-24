rm(list = ls())

setwd("C:/R/bachelorproject")

#load packages
package_load <- function(x) {
  for (i in x) {
    # require returns TRUE invisibly if it was able to load packages
    if ( ! require (i, character.only = TRUE)) {
      # IF package was not able to be loaded ten re-install
      install.packages(i, dependencies = T)
      
    }
    
  }
}

packages <- c("ncdf4","tidyverse", "chron", "rgdal", "readxl", "splitstackshape", "fastDummies",
              "wbstats", "pwt","pwt9", "data.table", "foreign", "plm", "stargazer", "R.utils")

package_load(packages)

#import original 

original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

#my one

mydata <- read_csv("./csv_files/cru2_1_tmp.csv")

#compare

table(mydata$iso3)
table(original$countryisocode)


cru2_check <- mydata %>% right_join(original, by = c("iso3" = "countryisocode", "years" = "year_actual"))

cru2_check <- cru2_check %>% mutate(tmp_diff = tmp - temp_all)

ggplot(cru2_check, aes(temp_all, tmp)) +
  geom_point(aes(colour = factor(country)))

ggplot(cru2_check, aes(years, tmp_diff)) +
  geom_point(aes(colour = factor(country)))

ggplot(cru2_check, aes(temp_all, tmp_diff)) +
  geom_point(aes(colour = factor(country)))
