##compare GDP in original data with possible datasets

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
      
    }
    
  }
}

packages <- c("ncdf4","tidyverse", "chron", "rgdal", "readxl", "splitstackshape", "fastDummies",
              "wbstats", "pwt","pwt9", "data.table", "foreign", "plm", "stargazer", "R.utils", "compare")

package_load(packages)

#import original data
climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

gdp_check <- climate_conflict_original %>% select(year_actual, country, gdp)

#import possibly used data

#Penn

data("pwt6.2")

pwt6.2 <- pwt6.2 %>% select(year,
                            country,
                            pop,
                            ppp, #purchasing power parity
                            cgdp, #real gross domestic product
                            rgdpl,
                            rgdptt)

pwt6.2$country <- as.character(pwt6.2$country)

unique(gdp_check$country[!gdp_check$country %in% pwt6.2$country]) # "Cote d`Ivoire" and "Congo, Dem. Rep."

unique(pwt6.2$country[!pwt6.2$country %in% gdp_check$country])

  #Congo, Dem. Rep. is "Congo, Democratic Republic"
  #Cote d`Ivoire is Cote d'Ivoire

gdp_check$country[gdp_check$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

pwt6.2$country[pwt6.2$country == "Congo, Democratic Republic"] <- "Congo, Dem. Rep."

#recalculate to per capita values in pwt dataframe

  #pwt6.2 <- pwt6.2 %>% mutate_at(vars(-pop, -country, -year, -ppp), `/`, quote(pop))

#join

pwt6.2$country <- as.character(pwt6.2$country)

gdp_check <- left_join(gdp_check, pwt6.2, by = c("country", "year_actual" = "year"))

view(gdp_check)

gdp_check[rowSums(is.na(gdp_check)) > 0 & gdp_check$year_actual <= 2002,]

# we have missing values recorded for libera , djibouti, angoola and lesotho in original data set
# but only for angola it is missing in pwt6.2.. why didn't authors use this data ? 

#plot

ggplot(gdp_check, aes(gdp, cgdp)) +
  geom_point(aes(colour = factor(country)))

ggplot(gdp_check, aes(gdp, rgdpl)) +
  geom_point(aes(colour = factor(country)))

ggplot(gdp_check, aes(gdp, rgdptt)) + 
  geom_point(aes(colour = factor(country)))

#rgdptt looks perfect...

#regress with rgdptt

summary(lm(gdp ~ rgdptt, data = gdp_check))

#it's only like in thousands.

gdp_check$rgdptt <- gdp_check$rgdptt/1000

rgdpttmodel <- lm(gdp ~ rgdptt, data = gdp_check)

par(mfrow=c(2,2))

plot(rgdpttmodel)

summary(rgdpttmodel)

plot(rgdpttmodel)
## get deviations

gdp_check <- gdp_check %>% mutate(gdp_rgdptt_diff = gdp - rgdptt,
                                  gdp_rgdptt_dev = gdp_rgdptt_diff/gdp)

ggplot(gdp_check, aes(gdp, gdp_rgdptt_dev)) + 
  geom_point(aes(colour = factor(country)))

rgdpttmodel2 <- lm(gdp ~ rgdptt + factor(country), data = gdp_check)

summary(rgdpttmodel2) #the countries make all the difference

ggplot(gdp_check, aes(year_actual, gdp_rgdptt_dev)) + 
  geom_point(aes(colour = factor(country)))

 # it also seems very randomly distributed across the years
 # which makes it unlogical that a possible conversion factor from 2000 USD to 1985 USD is missing.


rgdpttmodel3 <- lm(gdp ~ rgdptt + year_actual, data = gdp_check)

summary(rgdpttmodel3)

t.test(gdp_check$gdp_rgdptt_dev)

#bottom line: my computation is not the same , but super similar..
#and the deviation is not significantly different from zero, using one sample t test.

#why is it different ?
# actually, it should be more different as pwt6.2 gives out values in year 2000 USD instead of authors stated year 1985 USD.


## for WDI : downloaded manually because of non-functionality (for now) of wbstats package for WDI archive

wb_data <- read_csv("./data/gdp/wb/fae6d9d1-0c96-48c1-ab19-4150554b0811_Data.csv")

colnames(wb_data)

table(wb_data$`GDP per capita, PPP (constant 1987 international $) [NY.GDP.PCAP.PP.KD.87]`) # no values
table(wb_data$`GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`) #better
table(wb_data$`GDP per capita (current US$) [NY.GDP.PCAP.CD]`) #better
table(wb_data$`Population, total [SP.POP.TOTL]`) #better


wb_data <- wb_data %>% select(country = `Country Name`, 
                              year = Time, 
                              gdp_wb_1 = `GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]`,
                              gdp_wb_2 = `GDP per capita (current US$) [NY.GDP.PCAP.CD]`)

wb_data$gdp_wb_1[wb_data$gdp_wb_1 == ".."] <- NA
wb_data$gdp_wb_2[wb_data$gdp_wb_2 == ".."] <- NA

unique(gdp_check$country[!gdp_check$country %in% wb_data$country]) # "Congo, Republic of" 

unique(pwt6.2$country[!pwt6.2$country %in% gdp_check$country])

gdp_check$country[gdp_check$country == "Congo, Republic of"] <- "Congo"

gdp_check <- gdp_check %>% left_join(wb_data, by = c("country", "year_actual" = "year" ))

ggplot(gdp_check, aes(gdp, gdp_wb_2)) +
  geom_point(aes(colour = factor(country)))

## this looks baad...^^



## import my data

rm(list = ls())

mydata <- read_csv("./analytical_data/climate_conflict.csv")
mydata <- mydata %>% select(countryname, years, gdp)
original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")
original <- original %>% select(country, year_actual, gdp)
original$country[original$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

gdp_compare <- mydata %>% right_join(original, by = c("years" = "year_actual", "countryname" = "country"))
                                     
gdp_compare <- gdp_compare %>% mutate(gdp_diff = gdp.x - gdp.y,
                                      gdp_dev = gdp_diff/gdp.y)

t.test(gdp_compare$gdp_diff) # not significant from 0
t.test(gdp_compare$gdp_dev) # same

#see for gdp_diff
ggplot(gdp_compare, aes(gdp.x, gdp_diff)) + 
  geom_point(aes(colour = factor(countryname)))

ggplot(gdp_compare, aes(years, gdp_diff)) +
  geom_point(aes(colour = factor(countryname)))

# see for deviation

ggplot(gdp_compare, aes(gdp.x, gdp_dev)) +
  geom_point(aes(colour = factor(countryname)))
ggplot(gdp_compare, aes(years, gdp_diff)) +
  geom_point(aes(colour = factor(countryname)))

##bottom line : my difference is really small, and no country or country trends seen.
## except country/gdp-level trend in deviation: relatively big deviation for gdp around 1.5
# but still , small


view(gdp_compare[rowSums(is.na(gdp_compare)) > 0, ])

# quite some missing values. Liberia (from 1992), Lesotho and Djibouti, I have in my data, but not in original
#--> most likely because data from diffrent source : WB as well

# Congo (Republic of ) for 2004 in original but not in mine / not relevant time period tho

# other NA's in obth data sets.

