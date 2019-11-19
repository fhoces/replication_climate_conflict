### first run climate_conflict_rep_master.R to have the models in the environment . now start here

## get clustered standard errors for all models for output in stargazer

cluster_se_table1_model1 <- as.vector(summary(table1_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_table1_model2 <- as.vector(summary(table1_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_table1_model3 <- as.vector(summary(table1_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_tableS1_model1 <- as.vector(summary(tableS1_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model2 <- as.vector(summary(tableS1_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model3 <- as.vector(summary(tableS1_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model4 <- as.vector(summary(tableS1_model4, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model5 <- as.vector(summary(tableS1_model5, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model6 <- as.vector(summary(tableS1_model6, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model7 <- as.vector(summary(tableS1_model7, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS1_model8 <- as.vector(summary(tableS1_model8, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_tableS2_model1 <- as.vector(summary(tableS2_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS2_model2 <- as.vector(summary(tableS2_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS2_model3 <- as.vector(summary(tableS2_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS2_model4 <- as.vector(summary(tableS2_model4, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS2_model5 <- as.vector(summary(tableS2_model5, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS2_model6 <- as.vector(summary(tableS2_model6, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_tableS4_model1 <- as.vector(summary(tableS4_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS4_model2 <- as.vector(summary(tableS4_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS4_model3 <- as.vector(summary(tableS4_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS4_model4 <- as.vector(summary(tableS4_model4, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_tableS5_model1 <- as.vector(summary(tableS5_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS5_model2 <- as.vector(summary(tableS5_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_tableS61_model1 <- as.vector(summary(tableS6_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS61_model2 <- as.vector(summary(tableS6_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS61_model3 <- as.vector(summary(tableS6_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS61_model4 <- as.vector(summary(tableS6_model4, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS61_model5 <- as.vector(summary(tableS6_model5, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_tableS8_model1 <- as.vector(summary(tableS8_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS8_model2 <- as.vector(summary(tableS8_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS8_model3 <- as.vector(summary(tableS8_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_tableS8_model4 <- as.vector(summary(tableS8_model4, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_R_table1_model1 <- as.vector(summary(R_table1_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R_table1_model2 <- as.vector(summary(R_table1_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R_table1_model3 <- as.vector(summary(R_table1_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_R2_table1_model1 <- as.vector(summary(R2_table1_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R2_table1_model2 <- as.vector(summary(R2_table1_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R2_table1_model3 <- as.vector(summary(R2_table1_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])

cluster_se_R2_tableS4_model1 <- as.vector(summary(R2_tableS4_model1, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R2_tableS4_model2 <- as.vector(summary(R2_tableS4_model2, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R2_tableS4_model3 <- as.vector(summary(R2_tableS4_model3, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])
cluster_se_R2_tableS4_model4 <- as.vector(summary(R2_tableS4_model4, robust = T, cluster = c("iso3"))$coefficients[,"Std. Error"])


stargazer(table1_model1, table1_model2, table1_model3, style = "qje", omit = "iso3", font.size = "tiny", omit.stat = "f")

stargazer(table1_model1, table1_model2, table1_model3, style = "qje", omit = "iso3", font.size = "tiny", omit.stat = "f",se = list(cluster_se_table1_model1, cluster_se_table1_model2, cluster_se_table1_model3))


coeftest(table1_model1, vcov=vcovHC(table1_model1,type="HC0",cluster="iso3"))
