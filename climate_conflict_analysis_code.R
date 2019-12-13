### constructing the output published in my thesis (all the tables)
# note: to construct figure 1 in my thesis, one has to download, unzip and place in working directory the original replication folder of the authors of the paper I analized

## setting up workspace

rm(list = ls())
setwd("C:/R/richter_climate_conflict_replication_exercise")
options(scipen = 999)
subfolder_names <- c("analysis_data", "raw_data")

for (i in 1:length(subfolder_names)){
  folder<-dir.create(paste0("./",subfolder_names[i]))
}
subfolder_names2 <- c("polity", "gadm", "conflict", "cru_data", "gpcp", "gdp")
for (i in 1:length(subfolder_names2)){
  folder<-dir.create(paste0("./data/",subfolder_names2[i]))
}

# load packages

package_load <- function(x) {
  for (i in x) {
    
    # require returns TRUE invisibly if it was able to load packages
    
    if ( ! require (i, character.only = TRUE)) {
      
      # IF package was not able to be loaded ten re-install
      
      install.packages(i, dependencies = T)
    }
    
  }
}

packages <- c("tidyverse", "rgdal", "readxl","data.table", "foreign", "plm", 
              "stargazer", "R.utils", "compare", "lmtest", "sandwich", "tikzDevice")

package_load(packages)

theme_new <- function(base_size = 9,
                      base_family = "",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.5)),
      panel.grid.major = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted"),   
      panel.grid.minor = element_line(
        rgb(105, 105, 105, maxColorValue = 255),
        linetype = "dotted", 
        size = rel(2)),   
      
      complete = TRUE
    )
}

theme_set(theme_new())

# load orginal 

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

# load my analysis data

climate_conflict <- read_csv("./analysis_data/climate_conflict.csv")

climate_conflict_alternative <- read_csv("./analysis_data/climate_conflict_alternativecountryset.csv")

# country value harmonization
# ANALYTICAL CHOICE OF TYPE VALUE HARMONIZATION. FIRST RECORDED HERE.

unique(climate_conflict_original$country[!climate_conflict_original$country %in% climate_conflict$countryname]) # Cote d`Ivoire 

unique(climate_conflict$countryname[!climate_conflict$countryname %in% climate_conflict_original$country])


climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

# we left-join ... we seize down to original data obserations, whereas in our data sample there's more observations.
# ANALYTICAL CHOICE OF TYPE DATA RESHAPING. FIRST RECORDED HERE.

var_check <- climate_conflict_original %>% left_join(climate_conflict, by = c("year_actual" = "years" , "country" = "countryname"))


# figure 1


tikz(file = "./tikz/climateversionscompare.tex", width = 2.2, height = 2.2)

climateplot1 <- ggplot(var_check, aes(tmp, temp_all)) +
  geom_point(aes(colour = factor(country)), size = 0.5) +
  geom_abline(intercept = 0, slope = 1, colour="black", size=0.5)+
  labs(x = "Temperature in CRU 4.03", y = "Temperature in CRU 2.1" )+
  theme(legend.position = "none")

climateplot2 <- ggplot(var_check, aes(pre, prec_all)) +
  geom_point(aes(colour = factor(country)), size = 0.5) +
  geom_abline(intercept = 0, slope = 1,colour="black", size=0.5) +
  labs(x = "Precipitation in CRU 4.03", y= "Precipitation in CRU 2.1")+
  theme(legend.position = "none")

climateplot1
climateplot2
dev.off()


## regressions

# ANALYTICAL CHOICE OF TYPE ASSIGN DATA CLASS. FIRST RECORDED HERE.

climate_conflict$years <- as.numeric(climate_conflict$years) # need the numeric value of years for interaction term


# ANALYTICAL CHOICE OF TYPE REGRESSION FUNCTION.
# we use linear regression model and the base R function lm() for all regressions! 

# note : i define the var tmp, tmp_lag as main independent vars so won't mark them as controls.


# ANALYTICAL CHOICE MADE OF TYPE ADJUSTMENT OF STANDARD ERRORS.
# cluster robust standard errors, computed by sandwich package, for all models (unless otherwise stated)

# ANALYTICAL CHOICE OF TYPE CONTROLS. USING COUNTRY FE AND COUNTRY TIME TRENDS ALL THE TIME UNLESS OTHERWEISE STATED. FIRST RECORDED HERE.

## table 1


table1_model1 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                    data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE. 

table1_model2 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                    data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

table1_model3 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + gdp_lag + polity2_lag + factor(iso3)*years,
                    data = climate_conflict)

cov_table1_model1 <- vcovCL(table1_model1, cluster = climate_conflict$iso3)

cov_table1_model2 <- vcovCL(table1_model2, cluster = climate_conflict$iso3)

cov_table1_model3 <- vcovCL(table1_model3, cluster = climate_conflict$iso3)

clusterse_table1_model1 <- sqrt(diag(cov_table1_model1))

clusterse_table1_model2 <- sqrt(diag(cov_table1_model2))

clusterse_table1_model3 <- sqrt(diag(cov_table1_model3))


stargazer(table1_model1, table1_model2, table1_model3, se = list(clusterse_table1_model1, clusterse_table1_model2, clusterse_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), title = "Reproduction Result of Output Table 1.") 

## robustness 1 

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

R_table1_model1 <- lm(conflict ~ tmp_wrld_simpl + tmp_wrld_simpl_lag + factor(iso3)*years,
                      data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 145.

R_table1_model2 <- lm(conflict ~ tmp_wrld_simpl + tmp_wrld_simpl_lag + pre_wrld_simpl + pre_wrld_simpl_lag + factor(iso3)*years,
                      data = climate_conflict)

names(R_table1_model1$coefficients)[names(R_table1_model1$coefficients) == "tmp_wrld_simpl"] <- "tmp"
names(R_table1_model1$coefficients)[names(R_table1_model1$coefficients) == "tmp_wrld_simpl_lag"] <- "tmp_lag"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "tmp_wrld_simpl"] <- "tmp"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "tmp_wrld_simpl_lag"] <- "tmp_lag"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "pre_wrld_simpl"] <-  "pre"
names(R_table1_model2$coefficients)[names(R_table1_model2$coefficients) == "pre_wrld_simpl_lag"] <- "pre_lag"

cov_R_table1_model1 <- vcovCL(R_table1_model1, cluster = climate_conflict$iso3)

cov_R_table1_model2 <- vcovCL(R_table1_model2, cluster = climate_conflict$iso3)

clusterse_R_table1_model1 <- sqrt(diag(cov_R_table1_model1))

clusterse_R_table1_model2 <- sqrt(diag(cov_R_table1_model2))


stargazer(table1_model1, table1_model2, R_table1_model1, R_table1_model2, se = list(clusterse_table1_model1, clusterse_table1_model2, clusterse_R_table1_model1, clusterse_R_table1_model2) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)"), column.labels = c("GADM", "wrld\\_simpl"), column.separate = c(2,2), title = "Robustness Check 1: Comparison of the outcomes of two different methods of aggregating the spatial climate data: Models 1 and 2 use the GADM country boarder data, Models 3 and 4 the wrld\\_simpl from maptools instead.") 

# robustness 2 -> gdp and polity old vs gdp and polity new

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

R_table1_model3 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + gdp_pwt9_lag + polity2_2018_lag +factor(iso3)*years,
                      data = climate_conflict)

names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "gdp_pwt9_lag"] <-  "gdp_lag"
names(R_table1_model3$coefficients)[names(R_table1_model3$coefficients) == "polity2_2018_lag"] <- "polity2_lag"

cov_R_table1_model3 <- vcovCL(R_table1_model3, cluster = climate_conflict$iso3)

clusterse_R_table1_model3 <- sqrt(diag(cov_R_table1_model3))

stargazer(table1_model3, R_table1_model3, se = list(clusterse_table1_model3, clusterse_R_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), column.labels = c("original GDP \\& Polity", "Recent GDP \\& Polity"), column.separate = c(1,1), title = "Robustness Check 2: Generating Table 1, Model 3 using different Polity and GDP Meassures.") 

# Robustness 3 - additional countries.

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

R2_table1_model1 <- lm(conflict ~ tmp_alt_countries + tmp_alt_countries_lag + factor(iso3)*years,
                       data = climate_conflict_alternative)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 145.

R2_table1_model2 <- lm(conflict ~ tmp_alt_countries + tmp_alt_countries_lag + pre_alt_countries + pre_alt_countries_lag + factor(iso3)*years,
                       data = climate_conflict_alternative)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 150.

R2_table1_model3 <- lm(conflict ~ tmp_alt_countries + tmp_alt_countries_lag + pre_alt_countries + pre_alt_countries_lag + gdp_lag + polity2_lag +factor(iso3)*years,
                       data = climate_conflict_alternative)


cov_R2_table1_model1 <- vcovCL(R2_table1_model1, cluster = climate_conflict_alternative$iso3)
cov_R2_table1_model2 <- vcovCL(R2_table1_model2, cluster = climate_conflict_alternative$iso3)
cov_R2_table1_model3 <- vcovCL(R2_table1_model3, cluster = climate_conflict_alternative$iso3)

clusterse_R2_table1_model1 <- sqrt(diag(cov_R2_table1_model1))
clusterse_R2_table1_model2 <- sqrt(diag(cov_R2_table1_model2))
clusterse_R2_table1_model3 <- sqrt(diag(cov_R2_table1_model3))

stargazer(R2_table1_model1, R2_table1_model2, R2_table1_model3, se = list(clusterse_R2_table1_model1, clusterse_R2_table1_model2, clusterse_R2_table1_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), title = "Robustness Check 3a: Reproduction of Output Table 1, but broadening the range of countries from 41 to 49.") 

# ANALYTICAL CHOICE OF TYPE TREATMENT OF MISSING VALUES. FIRST RECORDED HERE.

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE.

R2_tableS4_model1 <- lm(conflict_onset ~ tmp_alt_countries + tmp_alt_countries_lag + factor(iso3)*years,
                        data = climate_conflict_alternative)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 145. 

R2_tableS4_model2 <- lm(conflict_onset ~ tmp_alt_countries + tmp_alt_countries_lag + pre_alt_countries + pre_alt_countries_lag + factor(iso3)*years,
                        data = climate_conflict_alternative)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

R2_tableS4_model3 <- lm(conflict_onset ~ tmp_alt_countries_diff + tmp_alt_countries_diff_lag + factor(iso3)*years,
                        data = climate_conflict_alternative)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

R2_tableS4_model4 <- lm(conflict_onset ~ tmp_alt_countries_diff + tmp_alt_countries_diff_lag + pre_alt_countries_diff + pre_alt_countries_diff_lag + factor(iso3)*years,
                        data = climate_conflict_alternative)

cov_R2_tableS4_model1 <- vcovCL(R2_tableS4_model1, cluster = climate_conflict_alternative$iso3)
cov_R2_tableS4_model2 <- vcovCL(R2_tableS4_model2, cluster = climate_conflict_alternative$iso3)
cov_R2_tableS4_model3 <- vcovCL(R2_tableS4_model3, cluster = climate_conflict_alternative$iso3)
cov_R2_tableS4_model4 <- vcovCL(R2_tableS4_model4, cluster = climate_conflict_alternative$iso3)

clusterse_R2_tableS4_model1 <- sqrt(diag(cov_R2_tableS4_model1))
clusterse_R2_tableS4_model2 <- sqrt(diag(cov_R2_tableS4_model2))
clusterse_R2_tableS4_model3 <- sqrt(diag(cov_R2_tableS4_model3))
clusterse_R2_tableS4_model4 <- sqrt(diag(cov_R2_tableS4_model4))

stargazer(R2_tableS4_model1, R2_tableS4_model2, R2_tableS4_model3, R2_tableS4_model4, se = list(clusterse_R2_tableS4_model1, clusterse_R2_tableS4_model2, clusterse_R2_tableS4_model3, clusterse_R2_tableS4_model4) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Onset of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","Temperature diff\\(_{(t)}\\)","Temperature diff\\(_{(t-1)}\\)", "Precipitation diff\\(_{(t)}\\)", "Precipitation diff\\(_{(t-1)}\\)"), title = "Robustness Check 3b: Reproduction of Output Table S4, but broadening the range of countries from 41 to 49.") 


# Robustness 4 - non clustered standard errors.

# ANALYTICAL CHOICE OF TYPE STANDARD ERRORS. FIRST RECORDED HERE. 

stargazer(table1_model1, table1_model2, table1_model3, style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)","GDP\\(_{(t-1)}\\)", "Polity Score\\(_{(t-1)}\\)"), title = "Robustness Check 4: Reproduction Result of Ouput Table 1, but with ordinary standard errors instead of cluster robust.") 


# table S1 

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

tableS1_model1 <- lm(conflict ~ tmp + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

tableS1_model2 <- lm(conflict ~ tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS1_model3 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE.

tableS1_model4 <- lm(conflict ~ pre + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE.

tableS1_model5 <- lm(conflict ~ pre_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE.

tableS1_model6 <- lm(conflict ~ pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 144. 

tableS1_model7 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

tableS1_model8 <- lm(residuals(tableS1_model6) ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

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


stargazer(tableS1_model1, tableS1_model2, tableS1_model3, tableS1_model4, tableS1_model5, tableS1_model6, tableS1_model7, tableS1_model8, se = list(clusterse_tableS1_model1, clusterse_tableS1_model2, clusterse_tableS1_model3, clusterse_tableS1_model4, clusterse_tableS1_model5, clusterse_tableS1_model6, clusterse_tableS1_model7, clusterse_tableS1_model8) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = c("Incidence of Civil War\\(_{(t)}\\)", "Residuals from Model 6\\(_{(t)}\\)"), covariate.labels = c("Temperature\\(_{(t)}\\)","Temperature\\(_{(t-1)}\\)", "Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)"), title = "Reproduction Result of Ouput Table S1.") 

# Table S2

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE. 

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 144. 

tableS2_model1 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS2_model2 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + factor(iso3) + years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORED HERE.

tableS2_model3 <- lm(conflict ~ tmp_diff + tmp_diff_lag + pre_diff + pre_diff_lag + factor(iso3)*years, 
                     data = climate_conflict)

tableS2_model4 <- lm(conflict ~ tmp_diff + tmp_diff_lag + pre_diff + pre_diff_lag + factor(iso3) + years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE. 

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS2_model5 <- lm(conflict ~ tmp_difftrend + tmp_difftrend_lag + pre_difftrend + pre_difftrend_lag + factor(iso3)*years,
                     data = climate_conflict)

tableS2_model6 <- lm(conflict ~ tmp_difftrend + tmp_difftrend_lag + pre_difftrend + pre_difftrend_lag + factor(iso3) + years,
                     data = climate_conflict)

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

# ANALYTICAL CHOICE OF TYPE TREATMENT OF MISSING VALUES. FIRST RECORDED IN LINE 244.

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED IN LINE 246.

tableS4_model1 <- lm(conflict_onset ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 145. 

tableS4_model2 <- lm(conflict_onset ~ tmp + tmp_lag + pre + pre_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 256.

tableS4_model3 <- lm(conflict_onset ~ tmp_diff + tmp_diff_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 261.

tableS4_model4 <- lm(conflict_onset ~ tmp_diff + tmp_diff_lag + pre_diff + pre_diff_lag + factor(iso3)*years,
                     data = climate_conflict)

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

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS5_model1 <- lm(conflict ~ tmp + tmp_lag + tmp_lead + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS5_model2 <- lm(conflict ~ tmp + tmp_lag + tmp_lead + pre + pre_lag + pre_lead + factor(iso3)*years,
                     data = climate_conflict)

cov_tableS5_model1 <- vcovCL(tableS5_model1, cluster = climate_conflict$iso3)
cov_tableS5_model2 <- vcovCL(tableS5_model2, cluster = climate_conflict$iso3)

clusterse_tableS5_model1 <- sqrt(diag(cov_tableS5_model1))
clusterse_tableS5_model2 <- sqrt(diag(cov_tableS5_model2))

stargazer(tableS5_model1, tableS5_model2, se = list(clusterse_tableS5_model1, clusterse_tableS5_model2), style = "qje", omit = c("iso3", "years"), font.size = "small", omit.stat = c("f", "ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)", "Temperature\\(_{(t-1)}\\)", "Temperature\\(_{(t+1)}\\)","Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t-1)}\\)", "Precipitation\\(_{(t+1)}\\)"), title = "Reproduction Result of Output Table S5.")

# Table S6

# create table S6

tableS6_model1 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS6_model2 <- lm(conflict ~ tmp + tmp_lag + gdp_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS6_model3 <- lm(conflict ~ tmp + tmp_lag + polity2_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS6_model4 <- lm(conflict ~ tmp + tmp_lag + gdp_lag + polity2_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED IN LINE 145.

tableS6_model5 <- lm(conflict ~ tmp + tmp_lag + pre + pre_lag + gdp_lag + polity2_lag + factor(iso3)*years,
                     data = climate_conflict)

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

tableS8_model1 <- lm(conflict ~ tmp + tmp_lag + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE.

tableS8_model2 <- lm(conflict ~ tmp + tmp_square + tmp_lag + tmp_lag_square + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE KEY PARAMETERS. FIRST RECORDED HERE.

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE. 

tableS8_model3 <- lm(conflict ~ tmp + tmp_square + factor(iso3)*years,
                     data = climate_conflict)

# ANALYTICAL CHOICE OF TYPE CONTROLS. FIRST RECORDED HERE. 

tableS8_model4 <- lm(conflict ~ tmp + tmp_square + tmp_lag + tmp_lag_square + pre + pre_square + pre_lag + pre_lag_square + factor(iso3)*years,
                     data = climate_conflict)



cov_tableS8_model1 <- vcovCL(tableS8_model1, cluster = climate_conflict$iso3)
cov_tableS8_model2 <- vcovCL(tableS8_model2, cluster = climate_conflict$iso3)
cov_tableS8_model3 <- vcovCL(tableS8_model3, cluster = climate_conflict$iso3)
cov_tableS8_model4 <- vcovCL(tableS8_model4, cluster = climate_conflict$iso3)

clusterse_tableS8_model1 <- sqrt(diag(cov_tableS8_model1))
clusterse_tableS8_model2 <- sqrt(diag(cov_tableS8_model2))
clusterse_tableS8_model3 <- sqrt(diag(cov_tableS8_model3))
clusterse_tableS8_model4 <- sqrt(diag(cov_tableS8_model4))

stargazer(tableS8_model1, tableS8_model2, tableS8_model3, tableS8_model4, se = list(clusterse_tableS8_model1, clusterse_tableS8_model2, clusterse_tableS8_model3) , style = "qje", omit = c("iso3","years"), font.size = "small", omit.stat = c("f","ser"), dep.var.labels = "Incidence of Civil War\\(_{(t)}\\)", covariate.labels = c("Temperature\\(_{(t)}\\)", "Temperature\\(_{(t)^{2}}\\)", "Temperature\\(_{(t-1)}\\)", "Temperature\\(_{(t-1)^{2}}\\)","Precipitation\\(_{(t)}\\)", "Precipitation\\(_{(t)^{2}}\\)", "Precipitation\\(_{(t-1)}\\)", "Precipitation\\(_{(t-1)^{2}}\\)"), title = "Reproduction Result of Output Table S8.") 

