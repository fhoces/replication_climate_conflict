rm(list=ls())

setwd("C:/R/bachelorproject/cruimport")

crutmp81_90 <- readLines("cru_ts_2_10.1981-1990.tmp")
crutmp81_90 <- crutmp81_90[6:length(crutmp81_90)]

gridrefs <- grep(x = crutmp81_90, pattern = "^Grid-ref", value=TRUE)
head(gridrefs)

gridtable <- read.table(text = gridrefs)
names(gridtable) <- c("grid-ref", "x", "y")
gridtable$x <- with(gridtable, as.numeric(str_extract(gridtable$x, "[0-9]+")))
head(gridtable)
summary(gridtable)

gridtable <- subset(gridtable, select = c(x,y))
