##loading CRU 2.1 Data into R 

#setting up

library(sp)
library(raster)

rm(list=ls())

setwd("C:/R/bachelorproject/cruimport")

all_dat <- scan("./", skip = 5, what = "list")
