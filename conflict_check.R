##check conflict variables

rm(list = ls())

library(foreign)

setwd("C:/R/bachelorproject")

#import original file

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

climate_conflict_original <- climate_conflict_original %>% select(year_actual, country, war_prio_new, war_onset_new)
table(climate_conflict_original$year_actual)
#import my constructed data

conflict_check <- read_csv("./csv_files/climate_conflict.csv")

conflict_check <- conflict_check %>% select(years, countryname, conflict, conflict_onset )

table(conflict_check$years)

#join
unique(climate_conflict_original$country[!climate_conflict_original$country %in% conflict_check$countryname]) # Cote d`Ivoire again

unique(conflict_check$countryname[!conflict_check$countryname %in% climate_conflict_original$country])

climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

conflict_check <- conflict_check %>% right_join(climate_conflict_original, by = c("years" = "year_actual", "countryname" = "country"))

## note that we right-joined ... we seize down to original data obserations, whereas in our data sample there's more observations.

conflict_check[is.na(conflict_check$conflict),] #no missing values in conflict

view(conflict_check[rowSums(is.na(conflict_check)) > 0 ,]) #no missing values 

table(conflict_check$war_onset_new)

conflict_check[!conflict_check$conflict == conflict_check$war_prio_new,]

view(conflict_check)

conflict_check <- conflict_check %>% mutate(conflict_diff = ifelse(conflict == war_prio_new, 0, 1),
                                            onset_diff = ifelse(conflict_onset == war_onset_new, 0, 1))

table(conflict_check$conflict_diff) # all obs. similar 

table(conflict_check$onset_diff) # 970 similar, the rest NA

#check for NA values in onset

conflict_check[is.na(conflict_check$onset_diff) & (!is.na(conflict_check$conflict_onset) | !is.na(conflict_check$war_onset_new)),]
#returns values where either original or mine =0 for onset , but not other way around

view(conflict_check[is.na(conflict_check$onset_diff), ])

# it doesn't make any sense that the three obs.
# years 1998:2000 in congo, dem. rep. 
# are coded as 0 in the original files, instead of NA 

#this is the only difference between mine and original dataset 

# decision to be made which to use.

#case closed




