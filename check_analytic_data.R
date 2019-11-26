## check difference on analytic data -hence: variables

## set up

rm(list = ls())

setwd("C:/R/bachelorproject")

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

packages <- c("ncdf4","tidyverse", "chron", "rgdal", "readxl", "splitstackshape", "fastDummies",
              "wbstats", "pwt","pwt9", "data.table", "foreign", "plm", "stargazer", "R.utils", "compare", "tikzDevice")

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

# import orginal 

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

# import our data

mydata <- read_csv("./analysis_data/climate_conflict.csv")

# country value harmonization

unique(climate_conflict_original$country[!climate_conflict_original$country %in% mydata$countryname]) # Cote d`Ivoire 

unique(mydata$countryname[!mydata$countryname %in% climate_conflict_original$country])


climate_conflict_original$country[climate_conflict_original$country == "Cote d`Ivoire"] <- "Cote d'Ivoire"

## we left-join ... we seize down to original data obserations, whereas in our data sample there's more observations.

var_check <- climate_conflict_original %>% left_join(mydata, by = c("year_actual" = "years" , "country" = "countryname"))


### climate check
tikz(file = "firstclimateplots.tex", width = 2.2, height = 2.2)

climateplot1 <- ggplot(var_check, aes(tmp, temp_all)) +
                geom_point(aes(colour = factor(country)), size = 0.5) +
                geom_abline(intercept = 0, slope = 1, colour="black", size=0.5)+
                labs(x = "Temperature in CRU 4.03", y = "Temperature in CRU 2.1" )+
                theme(legend.position = "none")

climateplot2 <- ggplot(var_check, aes(pre, prec_all)) +
                geom_point(aes(colour = factor(country)), size = 0.5) +
                geom_abline(intercept = 0, slope = 1,colour="black", size=0.5) +
                labs(x = "Precipitaion in CRU 4.03", y= "Precipiation in CRU 2.1")+
                theme(legend.position = "none")

climateplot1
climateplot2
dev.off()
# create differences mine and original

var_check <- var_check %>% mutate(tmp_temp_diff = tmp - temp_all,
                                          tmp_temp_dev = tmp_temp_diff/temp_all,
                                          pre_prec_diff = pre - prec_all,
                                          pre_prec_dev = pre_prec_diff/prec_all)


ggplot(var_check, aes(temp_all, tmp_temp_dev)) +
  geom_point(aes(colour = factor(country)))

ggplot(var_check, aes(year_actual, tmp_temp_dev)) +
  geom_point(aes(colour = factor(country)))

# regression 

summary(lm(tmp_temp_dev ~ temp_all + factor(country),
           data = var_check))

summary(lm(abs(tmp_temp_dev) ~ temp_all + factor(country),
           data = var_check))

# small influcence of temperature 
# note that the regressor for temp_all estimates the effect of 1 unit increase in temperature on deviation
# the difference between 1st qu. and 3rd qu. in our temperature observations is smaller than 5 units

# connection tmp_diff and conflict ?--> running logit model

summary(glm(conflict ~ abs(tmp_temp_dev) ,
            family = binomial(link = "logit"),
           data = var_check))

ggplot(var_check, aes(prec_all, pre_prec_dev)) +
  geom_point(aes(colour = factor(country)))

ggplot(var_check, aes(year_actual, pre_prec_dev)) +
  geom_point(aes(colour = factor(country)))

summary(lm(pre_prec_dev ~ prec_all + factor(country),
           data = var_check))
summary(lm(abs(pre_prec_dev) ~ prec_all + factor(country),
           data = var_check))

## diff and dev (from climate) check

summary(var_check$tmp_diff)
summary(var_check$temp_all_dif)
# similar range and mean/median, original rage slightly bigger

summary(var_check$pre_diff)
summary(var_check$prec_all_dif)
#same for precipitation

summary(var_check$tmp_difftrend)
summary(var_check$cru_temp_diftrend)

summary(var_check$pre_difftrend)
summary(var_check$cru_prec_diftrend)

# also similar range for dev from trend


# check tmp differences 

ggplot(var_check, aes(tmp_diff, temp_all_dif))+
  geom_point(aes(colour = factor(country))) # they're different , but scattered around similarity, no country trend
ggplot(var_check, aes(tmp_diff, temp_all_dif))+
  geom_point(aes(colour = factor(year_actual))) # no time trend

# check for precipitation

ggplot(var_check, aes(pre_diff, prec_all_dif))+
  geom_point(aes(colour = factor(country)))

ggplot(var_check, aes(pre_diff, prec_all_dif))+
  geom_point(aes(colour = factor(year_actual)))

# we obtain same results like for tmp --> different in CRU 2 compared to CRU 4 , but not correlated with country or year

# now do the same for the deviation from trend

ggplot(var_check, aes(cru_temp_diftrend,tmp_difftrend)) +
  geom_point(aes(colour = factor(country)))

ggplot(var_check, aes(cru_temp_diftrend, tmp_difftrend)) +
  geom_point(aes(colour = factor(year_actual)))

ggplot(var_check, aes(cru_prec_diftrend,pre_difftrend)) +
  geom_point(aes(colour = factor(country)))

ggplot(var_check, aes(cru_prec_diftrend,pre_difftrend)) +
  geom_point(aes(colour = factor(year_actual)))

## conflict check

var_check[is.na(var_check$conflict),] # no missing values in conflict

var_check[!var_check$conflict == var_check$war_prio_new,] # all conflict obs. same

table(var_check$war_onset_new) # missing values (because we manually defined them!)

# need another approach than for conflict_onset

var_check <- var_check %>% mutate(onset_diff = ifelse(conflict_onset == war_onset_new, 0, 1))

table(var_check$onset_diff) # 972 similar, the rest NA

#check for NA values in onset

var_check[is.na(var_check$onset_diff) & (!is.na(var_check$conflict_onset) | !is.na(var_check$war_onset_new)),]

#returns values where either original or mine =0 for onset , but not other way around


view(var_check[is.na(var_check$onset_diff), ])

# it doesn't make any sense that the three obs.
# years 1998:2000 in congo, dem. rep. 
# are coded as 0 in the original files, instead of NA 

# this is the only difference between mine and original dataset 


## gdp check

var_check[1, grepl("gdp", names(var_check))]

ggplot(var_check, aes(gdp_l, gdp_lag)) +
  geom_point(aes(colour = factor(country)))

view(var_check[rowSums(is.na(var_check)) > 0 & var_check$year_actual <= 2002, c(1, grepl("gdp", names(var_check)))])

# some missing values. Liberia (from 1992), Lesotho and Djibouti, I have in my data, but not in original
#--> most likely because data from diffrent source : WB as well

## polity check

compare(var_check$polity2_lag.x, var_check$polity2_lag.y) # same

ggplot(var_check, aes(tmp, temp_all)) +
  geom_point(aes(colour = factor(country)), size = 0.5) +
  geom_smooth(method='lm', formula= y~x) +
  labs(x = "Temperature in CRU 4.03", y = "Temperature in CRU 2.1" )+
  theme(legend.position = "none")
