### first run climate_conflict_rep_master.R to have the models in the environment . now start here
# load necessary packages for importing the function


### table 1

models <- c("table1_model1",
           "table1_model2",
           "table1_model3",
           "tableS1_model1",
           "tableS1_model2",
           "tableS1_model3",
           "tableS1_model4",
           "tableS1_model5",
           "tableS1_model6",
           "tableS1_model7",
           "tableS1_model8",
           "tableS2_model1",
           "tableS2_model2",
           "tableS2_model3",
           "tableS2_model4",
           "tableS2_model5",
           "tableS2_model6",
           "tableS4_model1",
           "tableS4_model2",
           "tableS4_model3",
           "tableS4_model4",
           "tableS5_model1",
           "tableS5_model2",
           "tableS6_model1",
           "tableS6_model2",
           "tableS6_model3",
           "tableS6_model4",
           "tableS6_model5",
           "tableS8_model1",
           "tableS8_model2",
           "tableS8_model3",
           "tableS8_model4",
           "R_table1_model1",
           "R_table1_model2",
           "R_table1_model3"
           )


models <- data.frame(models, stringsAsFactors = FALSE)
str(models)
for (i in 1:length(models)) {
  cov <- vcovHC(i)
  clusterse <- sqrt(diag(cov))
}

cov_table1_model1 <- vcovCL(table1_model1, cluster = climate_conflict$iso3)

cov_table1_model2 <- vcovCL(table1_model2, cluster = climate_conflict$iso3)

cov_table1_model3 <- vcovCL(table1_model3, cluster = climate_conflict$iso3)

clusterse_table1_model1 <- sqrt(diag(cov_table1_model1))

clusterse_table1_model2 <- sqrt(diag(cov_table1_model2))

clusterse_table1_model3 <- sqrt(diag(cov_table1_model3))


# table 1 with sandwich clustered SE 

cov_table1_model1 <- vcovCL(table1_model1, cluster = climate_conflict$iso3)

cov_table1_model2 <- vcovCL(table1_model2, cluster = climate_conflict$iso3)

cov_table1_model3 <- vcovCL(table1_model3, cluster = climate_conflict$iso3)

clusterse_table1_model1 <- sqrt(diag(cov_table1_model1))

clusterse_table1_model2 <- sqrt(diag(cov_table1_model2))

clusterse_table1_model3 <- sqrt(diag(cov_table1_model3))

stargazer(table1_model1, table1_model2, table1_model3, se = list(clusterse_table1_model1, clusterse_table1_model2, clusterse_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-)}\\)", "Polity Score\\(_{(t-)}\\)"), title = "Reproduction Result of Output Table 1.") 

# robustness 1 with clustered SE 
names(R_table1_model2$coefficients)
names(R_table1_model1$coefficients)[names(R_table1_model1$coefficients) == "tmp.y"] <- "tmp"
names(R_table1_model1$coefficients)[names(R_table1_model1$coefficients) == "tmp_lag.y"] <- "tmp_lag"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "tmp.y"] <- "tmp"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "tmp_lag.y"] <- "tmp_lag"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "pre.y"] <-  "pre"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "pre_lag.y"] <- "pre_lag"

cov_R_table1_model1 <- vcovCL(R_table1_model1, cluster = climate_conflict$iso3)

cov_R_table1_model2 <- vcovCL(R_table1_model2, cluster = climate_conflict$iso3)

clusterse_R_table1_model1 <- sqrt(diag(cov_R_table1_model1))

clusterse_R_table1_model2 <- sqrt(diag(cov_R_table1_model2))


stargazer(table1_model1, table1_model2, R_table1_model1, R_table1_model2, se = list(clusterse_table1_model1, clusterse_table1_model2, clusterse_R_table1_model1, clusterse_R_table1_model2) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)"), column.labels = c("GADM", "wrld\\_simpl"), column.separate = c(2,2), title = "Robustness Check 1: Comparison of the outcomes of two different methods of aggregating the spatial climate data: Models 1 and 2 use the GADM country boarder data, Models 3 and 4 the wrld\\_simpl from maptools instead.") 

# robustness 2 -> gdp and polity old vs gdp and polity new

names(R_table1_model3$coefficients)
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "tmp.y"] <- "tmp"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "tmp_lag.y"] <- "tmp_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "pre.y"] <-  "pre"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "pre_lag.y"] <- "pre_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "GDP_pwt9_lag"] <-  "gdp_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "polity2_2018_lag"] <- "polity2_lag"

cov_R_table1_model3 <- vcovCL(R_table1_model3, cluster = climate_conflict$iso3)

clusterse_R_table1_model3 <- sqrt(diag(cov_R_table1_model3))

stargazer(table1_model3, R_table1_model3, se = list(clusterse_table1_model3, clusterse_R_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), column.labels = c("original GDP \\& Polity", "Recent GDP \\& Polity"), column.separate = c(1,1), title = "Robustness Check 2: Generating Table 1, Model 3 using different Polity and GDP Meassures.") 

# Robustness 3 - additional countries.
cov_R2_table1_model1 <- vcovCL(R2_table1_model1, cluster = climate_conflict_alternative$iso3)
cov_R2_table1_model2 <- vcovCL(R2_table1_model2, cluster = climate_conflict_alternative$iso3)
cov_R2_table1_model3 <- vcovCL(R2_table1_model3, cluster = climate_conflict_alternative$iso3)

clusterse_R2_table1_model1 <- sqrt(diag(cov_R2_table1_model1))
clusterse_R2_table1_model2 <- sqrt(diag(cov_R2_table1_model2))
clusterse_R2_table1_model3 <- sqrt(diag(cov_R2_table1_model3))

stargazer(R2_table1_model1, R2_table1_model2, R2_table1_model3, se = list(clusterse_R2_table1_model1, clusterse_R2_table1_model2, clusterse_R2_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation", "Precipitation\\(_{(t)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), title = "Robustness Check 3a: Reproduction of Output Table 1, but broadening the range of countries from 41 to 53.") 

cov_R2_tableS4_model1 <- vcovCL(R2_tableS4_model1, cluster = climate_conflict_alternative$iso3)
cov_R2_tableS4_model2 <- vcovCL(R2_tableS4_model2, cluster = climate_conflict_alternative$iso3)
cov_R2_tableS4_model3 <- vcovCL(R2_tableS4_model3, cluster = climate_conflict_alternative$iso3)
cov_R2_tableS4_model4 <- vcovCL(R2_tableS4_model4, cluster = climate_conflict_alternative$iso3)

clusterse_R2_tableS4_model1 <- sqrt(diag(cov_R2_tableS4_model1))
clusterse_R2_tableS4_model2 <- sqrt(diag(cov_R2_tableS4_model2))
clusterse_R2_tableS4_model3 <- sqrt(diag(cov_R2_tableS4_model3))
clusterse_R2_tableS4_model4 <- sqrt(diag(cov_R2_tableS4_model4))

stargazer(R2_tableS4_model1, R2_tableS4_model2, R2_tableS4_model3, R2_tableS4_model4, se = list(clusterse_R2_tableS4_model1, clusterse_R2_tableS4_model2, clusterse_R2_tableS4_model3, clusterse_R2_tableS4_model4) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Onset of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","Temperature diff\\(_{(t)}\\)","Temperature diff\\(_{(t-1)}\\)", "Precipitation diff\\(_{(t)}\\)", "Precipitation diff\\(_{(t-1)}\\)"), title = "Robustness Check 3b: Reproduction of Output Table S4, but broadening the range of countries from 41 to 53.") 


# Robustness 4 - non clustered standard errors.

stargazer(table1_model1, table1_model2, table1_model3, style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), title = "Robustness Check 4: Reproduction Result of Ouput Table 1, but with ordinary standard errors instead of cluster robust.") 


# table S1 

cov_tableS1_model1 <- vcovCL(tableS1_model1, cluster = climate_conflict$iso3)
cov_tableS1_model2 <- vcovCL(tableS1_model2, cluster = climate_conflict$iso3)
cov_tableS1_model3 <- vcovCL(tableS1_model3, cluster = climate_conflict$iso3)
cov_tableS1_model4 <- vcovCL(tableS1_model4, cluster = climate_conflict$iso3)
cov_tableS1_model5 <- vcovCL(tableS1_model5, cluster = climate_conflict$iso3)
cov_tableS1_model6 <- vcovCL(tableS1_model6, cluster = climate_conflict$iso3)
cov_tableS1_model7 <- vcovCL(tableS1_model7, cluster = climate_conflict$iso3)
cov_tableS1_model8 <- vcovCL(tableS1_model8, cluster = climate_conflict$iso3)

clusterse_tableS1_model1 <- sqrt(diag(cov_tableS1_model1))
clusterse_tableS1_model2 <- sqrt(diag(cov_tableS1_model2))
clusterse_tableS1_model3 <- sqrt(diag(cov_tableS1_model3))
clusterse_tableS1_model4 <- sqrt(diag(cov_tableS1_model4))
clusterse_tableS1_model5 <- sqrt(diag(cov_tableS1_model5))
clusterse_tableS1_model6 <- sqrt(diag(cov_tableS1_model6))
clusterse_tableS1_model7 <- sqrt(diag(cov_tableS1_model7))
clusterse_tableS1_model8 <- sqrt(diag(cov_tableS1_model8))


stargazer(tableS1_model1, tableS1_model2, tableS1_model3, tableS1_model4, tableS1_model5, tableS1_model6, tableS1_model7, tableS1_model8, se = list(clusterse_tableS1_model1, clusterse_tableS1_model2, clusterse_tableS1_model3, clusterse_tableS1_model4, clusterse_tableS1_model5, clusterse_tableS1_model6, clusterse_tableS1_model7, clusterse_tableS1_model8) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = c("Incidence of Civil War\\(_{(t)}\\)", "Residuals from Model 6"), covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)"), title = "Reproduction Result of Ouput Table S1.") 

# Table S2

cov_tableS2_model1 <- vcovCL(tableS2_model1, cluster = climate_conflict$iso3)
cov_tableS2_model2 <- vcovCL(tableS2_model2, cluster = climate_conflict$iso3)
cov_tableS2_model3 <- vcovCL(tableS2_model3, cluster = climate_conflict$iso3)
cov_tableS2_model4 <- vcovCL(tableS2_model4, cluster = climate_conflict$iso3)
cov_tableS2_model5 <- vcovCL(tableS2_model5, cluster = climate_conflict$iso3)
cov_tableS2_model6 <- vcovCL(tableS2_model6, cluster = climate_conflict$iso3)

clusterse_tableS2_model1 <- sqrt(diag(cov_tableS2_model1))
clusterse_tableS2_model2 <- sqrt(diag(cov_tableS2_model2))
clusterse_tableS2_model3 <- sqrt(diag(cov_tableS2_model3))
clusterse_tableS2_model4 <- sqrt(diag(cov_tableS2_model4))
clusterse_tableS2_model5 <- sqrt(diag(cov_tableS2_model5))
clusterse_tableS2_model6 <- sqrt(diag(cov_tableS2_model6))

stargazer(tableS2_model1, tableS2_model2, tableS2_model3, tableS2_model4, tableS2_model5, tableS2_model6, se = list(clusterse_tableS2_model1, clusterse_tableS2_model2, clusterse_tableS2_model3, clusterse_tableS2_model4, clusterse_tableS2_model5, clusterse_tableS2_model6) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","Temperature diff\\(_{(t)}\\)","Temperature diff\\(_{(t-1)}\\)", "Precipitation diff\\(_{(t)}\\)", "Precipitation diff\\(_{(t-1)}\\)","Temperature dev trend\\(_{(t)}\\)","Temperature dev trend\\(_{(t-1)}\\)", "Precipitation dev trend\\(_{(t)}\\)", "Precipitation dev trend\\(_{(t-1)}\\)"), title = "Reproduction Result of Output Table S2.") 

# Table S4

cov_tableS4_model1 <- vcovCL(tableS4_model1, cluster = climate_conflict$iso3)
cov_tableS4_model2 <- vcovCL(tableS4_model2, cluster = climate_conflict$iso3)
cov_tableS4_model3 <- vcovCL(tableS4_model3, cluster = climate_conflict$iso3)
cov_tableS4_model4 <- vcovCL(tableS4_model4, cluster = climate_conflict$iso3)

clusterse_tableS4_model1 <- sqrt(diag(cov_tableS4_model1))
clusterse_tableS4_model2 <- sqrt(diag(cov_tableS4_model2))
clusterse_tableS4_model3 <- sqrt(diag(cov_tableS4_model3))
clusterse_tableS4_model4 <- sqrt(diag(cov_tableS4_model4))

stargazer(tableS4_model1, tableS4_model2, tableS4_model3, tableS4_model4, se = list(clusterse_tableS4_model1, clusterse_tableS4_model2, clusterse_tableS4_model3, clusterse_tableS4_model4) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Onset of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","Temperature diff\\(_{(t)}\\)","Temperature diff\\(_{(t-1)}\\)", "Precipitation diff\\(_{(t)}\\)", "Precipitation diff\\(_{(t-1)}\\)"), title = "Reproduction Result of Output Table S4.") 

# Table S5

cov_tableS5_model1 <- vcovCL(tableS5_model1, cluster = climate_conflict$iso3)
cov_tableS5_model2 <- vcovCL(tableS5_model2, cluster = climate_conflict$iso3)

clusterse_tableS5_model1 <- sqrt(diag(cov_tableS5_model1))
clusterse_tableS5_model2 <- sqrt(diag(cov_tableS5_model2))

stargazer(tableS5_model1, tableS5_model2, se = list(clusterse_tableS5_model1, clusterse_tableS5_model2), style = "qje", omit = c("iso3", "years"), font.size = "small", omit.stat = c("f", "ser"), dep.var.labels = "Incidence of civil conflict\\(_{t}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)", "Temperature\\(_{(t-1)}\\)", "Temperature\\(_{(t+1)}\\)","Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)", "Precipitation\\(_{(t+1)}\\)"), title = "Reproduction Result of Output Table S5.")

# Table S6

cov_tableS6_model1 <- vcovCL(tableS6_model1, cluster = climate_conflict$iso3)
cov_tableS6_model2 <- vcovCL(tableS6_model2, cluster = climate_conflict$iso3)
cov_tableS6_model3 <- vcovCL(tableS6_model3, cluster = climate_conflict$iso3)
cov_tableS6_model4 <- vcovCL(tableS6_model4, cluster = climate_conflict$iso3)
cov_tableS6_model5 <- vcovCL(tableS6_model5, cluster = climate_conflict$iso3)

clusterse_tableS6_model1 <- sqrt(diag(cov_tableS6_model1))
clusterse_tableS6_model2 <- sqrt(diag(cov_tableS6_model2))
clusterse_tableS6_model3 <- sqrt(diag(cov_tableS6_model3))
clusterse_tableS6_model4 <- sqrt(diag(cov_tableS6_model4))
clusterse_tableS6_model5 <- sqrt(diag(cov_tableS6_model5))

stargazer(tableS6_model1, tableS6_model2, tableS6_model3, tableS6_model4, tableS6_model5, se = list(clusterse_tableS6_model1, clusterse_tableS6_model2, clusterse_tableS6_model3, clusterse_tableS6_model4, clusterse_tableS6_model5) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), title = "Reproduction Result of Output Table S6.") 

# Table S8

cov_tableS8_model1 <- vcovCL(tableS8_model1, cluster = climate_conflict$iso3)
cov_tableS8_model2 <- vcovCL(tableS8_model2, cluster = climate_conflict$iso3)
cov_tableS8_model3 <- vcovCL(tableS8_model3, cluster = climate_conflict$iso3)
cov_tableS8_model4 <- vcovCL(tableS8_model4, cluster = climate_conflict$iso3)

clusterse_tableS8_model1 <- sqrt(diag(cov_tableS8_model1))
clusterse_tableS8_model2 <- sqrt(diag(cov_tableS8_model2))
clusterse_tableS8_model3 <- sqrt(diag(cov_tableS8_model3))
clusterse_tableS8_model4 <- sqrt(diag(cov_tableS8_model4))

stargazer(tableS8_model1, tableS8_model2, tableS8_model3, tableS8_model4, se = list(clusterse_tableS8_model1, clusterse_tableS8_model2, clusterse_tableS8_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)", "Temperature\\(_{(t)^{2}}\\)", "Temperature\\(_{(t-1)}\\)", "Temperature\\(_{(t-1)^{2}}\\)","Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t)^{2}}\\)", "Precipitation\\(_{(t-1)}\\)", "Precipitation\\(_{(t-1)^{2}}\\)"), title = "Reproduction Result of Output Table S8.") 

