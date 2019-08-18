rm(list=ls())

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/Bachelorarbeit/Pomeranz/Replication Thesis/burke - warming increases the risk of civil war in africa/DATA/gridded data second CRU")

processgrids <- function() {
  con <- file("./data/cru_ts_2_10.1981-1990small.tmp", "r")
  while {
    line = readLines(con, n = 1)
      if { startsWith("con", "grid", ignore.case=TRUE)
        
     # x,y = line.split... }
      tmp_mat = read.table(file, skip=n, nrows=10) 
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

processgrids()
