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


models <- data.frame(table1_model1, table1_model2)
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

stargazer(table1_model1, table1_model2, table1_model3, se = list(clusterse_table1_model1, clusterse_table1_model2, clusterse_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Conflict", covariate.labels = c("Temperature","Temperature\\(_{(t-1)}\\)", "Precipitation", "Precipiation\\(_{(t-1)}\\)","GDP", "Polity Score"), title = "Reproduction Result of Ouput Table 1.") 

# table R 1 with clustered SE 

cov_R_table1_model1 <- vcovCL(R_table1_model1, cluster = climate_conflict$iso3)

cov_R_table1_model2 <- vcovCL(R_table1_model2, cluster = climate_conflict$iso3)

clusterse_R_table1_model1 <- sqrt(diag(cov_R_table1_model1))

clusterse_R_table1_model2 <- sqrt(diag(cov_R_table1_model2))

names(R_table1_model2$coefficients)
names(R_table1_model1$coefficients)[names(R_table1_model1$coefficients) == "tmp.y"] <- "tmp"
names(R_table1_model1$coefficients)[names(R_table1_model1$coefficients) == "tmp_lag.y"] <- "tmp_lag"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "tmp.y"] <- "tmp"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "tmp_lag.y"] <- "tmp_lag"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "pre.y"] <-  "pre"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "pre_lag.y"] <- "pre_lag"

stargazer(table1_model1, table1_model2, R_table1_model1, R_table1_model2, se = list(clusterse_table1_model1, clusterse_table1_model2, clusterse_R_table1_model1, clusterse_R_table1_model2) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Conflict", covariate.labels = c("Temperature","Temperature\\(_{(t-1)}\\)", "Precipitation", "Precipiation\\(_{(t-1)}\\)"), column.labels = c("GADM", "wrld\\_simpl"), column.separate = c(2,2), title = "Comparison of the outcomes of two different methods of aggregating the spatial climate data: Models 1 and 2 use the GADM country boarder data, Models 3 and 4 the wrld\\_simpl from maptools instead.") 

# table R 2 -> gdp and polity old vs gdp and polity new

cov_R_table1_model3 <- vcovCL(R_table1_model3, cluster = climate_conflict$iso3)

clusterse_R_table1_model3 <- sqrt(diag(cov_R_table1_model3))

names(R_table1_model3$coefficients)
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "tmp.y"] <- "tmp"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "tmp_lag.y"] <- "tmp_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "pre.y"] <-  "pre"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "pre_lag.y"] <- "pre_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "GDP_pwt9_lag"] <-  "gdp_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "polity2_2018_lag"] <- "polity_lag"

stargazer(table1_model3, R_table1_model3, se = list(clusterse_table1_model3, clusterse_R_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Conflict", covariate.labels = c("Temperature","Temperature\\(_{(t-1)}\\)", "Precipitation", "Precipiation\\(_{(t-1)}\\)","GDP", "Polity Score"), column.labels = c("original GDP \\& Polity", "Recent GDP \\& Polity"), column.separate = c(1,1), title = "Comparison of the outcomes ") 

coeftest(table1_model1)

coeftest(table1_model1, vcov = vcovHC(table1_model1, type="HC1"))

coeftest(table1_model1, vcov = vcovCL(table1_model1, cluster = climate_conflict$iso3))

coeftest(R_table1_model3)


