##compare GDP in original data with possible datasets

rm(list = ls())

library(foreign)
library(tidyverse)
library(pwt)
library(ggplot2)

setwd("C:/R/bachelorproject")

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

pwt6.2 <- pwt6.2 %>% mutate_at(vars(-pop, -country, -year, -ppp), `/`, quote(pop))

#add ppp

 # pwt6.2 <- pwt6.2 %>% mutate_at(vars(-pop, -country, -year, -ppp), `*`, quote(ppp)) 
 # I leave this out as it seems very counterproductive when plotting the data

#join

pwt6.2$country <- as.character(pwt6.2$country)

gdp_check <- left_join(gdp_check, pwt6.2, by = c("country", "year_actual" = "year"))

view(gdp_check)

#plot

ggplot(gdp_check, aes(gdp, cgdp)) +
  geom_point(aes(colour = factor(country)))

ggplot(gdp_check, aes(gdp, rgdpl)) +
  geom_point(aes(colour = factor(country)))

ggplot(gdp_check, aes(gdp, rgdptt)) + 
  geom_point(aes(colour = factor(country)))

#cgdp is pretty straight .. but still...

#regress with cgdp

summary(lm(gdp ~ cgdp + factor(country), data = gdp_check))

#hm somoething is wrong here


## for WDI : downloaded manually because of non-functionality (for now) of wbstats package for WDI archive

wb_data <- read_csv("./data/gdp/wb/b0b84a41-4c82-4035-b645-baff91d87dc8_Data.csv")

colnames(wb_data)

wb_data <- wb_data %>% select(country = `Country Name`, 
                              year = Time, 
                              gdp_wb = `GDP, PPP (constant 1987 international $) [NY.GDP.MKTP.PP.KD.87]` )

unique(gdp_check$country[!gdp_check$country %in% wb_data$country]) # "Congo, Republic of" 

unique(pwt6.2$country[!pwt6.2$country %in% gdp_check$country])

gdp_check$country[gdp_check$country == "Congo, Republic of"] <- "Congo"

gdp_check <- gdp_check %>% left_join(gdp_check, wb_data, by = c("country", )
