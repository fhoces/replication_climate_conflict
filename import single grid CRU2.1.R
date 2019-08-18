## Importing single grid cells from CRU 2.1 dataset

rm(list=ls())
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Bachelorarbeit/Pomeranz/Replication Thesis/burke - warming increases the risk of civil war in africa/DATA/gridded data second CRU")

# Import single grid cell of data (1st grid)
tmp <- read.table("./data/cru_ts_2_10.1981-1990small.tmp", skip=6, nrows=10)

# Rename cols/rows
years <- 1981:1990
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(tmp) <- month
rownames(tmp) <- years

# convert to correct temp values
tmp <- tmp * 0.1

##maybe helpful for future reference
# Create a grid reference matrix
x <- c()
c <- rep(1:720, each=360)
r <- rep(360:1, times=720)
for(i in 1:length(c)) {
  x[i] <- paste(c[i], r[i], sep=",")
}
cru.grid <- matrix(x, nrow=360, ncol=720)
colnames(cru.grid) <- seq(from=-180, to=179.5, by=0.5)
rownames(cru.grid) <- seq(from=90, to=-89.5, by=-0.5)

#fix(cru.grid)

