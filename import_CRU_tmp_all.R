##loading CRU 2.1 Data into R 

#setting up

library(sp)
library(raster)

rm(list=ls())

setwd("C:/R/bachelorproject/cruimport")

all_dat <- scan("./cru_ts_2_10.1901-2002.tmp", skip = 5, what = "list")

#temperature january 1900

xs <- all_dat[seq(2, 37465029, 1203)]
xs <- gsub(",","", xs)
xs <- as.numeric(xs)
ys <- as.numeric(all_dat[seq(3, 37465029,1203)])

#put data in matrix : first column x value,
#second the y value and next column monthly temperature

mat <- matrix(c(xs,ys), ncol = 2, byrow = FALSE)
numb <- matrix(4:1203, ncol = 1)
numb <- apply(numb, 1, function(x) seq(x[1], 37465029, 1203))
mat <- cbind(mat, apply(numb, 2, function(x) as.numeric(all_dat[x])))
