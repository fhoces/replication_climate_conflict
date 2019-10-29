

###let's do some first regressions.

##reproduction original files 

#version1
countrytimetrends <- grep("^Iccyear", names(climate_conflict_original), value = T) 
countryfe <- grep("^iccode", names(climate_conflict_original), value = T)
formula1 <- reformulate(termlabels = c("temp_all",
                                       "temp_all_lag",
                                       countrytimetrends,
                                       countryfe),
                        
                        response = "war_prio_new")
model2 <- lm(formula1, data = climate_conflict_original)

blabla2 <- summary(model2)
summary(model2, robust = T, cluster = c("ccode"))

##looking good

#version2
climate_conflict_original$years <- as.numeric(climate_conflict_original$years) #has to be numeric for regression
model3 <- lm(war_prio_new ~ temp_all + temp_all_lag + factor(iso3)*years,data = climate_conflict_original)
blabla3 <- summary(model3)
summary(model3, robust = T, cluster = c("ccode"))

#same results


##use our data

climate_conflict$years <- as.numeric(climate_conflict$years) #need again the numeric value for interaction term

modelme <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years, data = climate_conflict)
blablame <- summary(modelme)
## results differ a bit.. tmp higher and tmp_lag much higher

modelme1 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
               data = climate_conflict)
summary(modelme1)

modelme2 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
               data = climate_conflict)
summary(modelme2)
#my results are a bit higher than original ones, again

plot(modelme1)
stargazer(model2, model3, modelme1)


blabla2$coefficients[ 1:3,]
blabla3$coefficients[ 1:3,]
blablame$coefficients[1:3,]

view(climate_conflict)
