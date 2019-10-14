### I want to find out how the climate diffs and deviations (from CRU data) have been computed

rm(list = ls())

setwd("C:/R/bachelorproject")

library(MASS)
library(foreign)
library(tidyverse)
library(broom)
library(ggplot2)

climate_conflict_original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")

## looking at diff's first

diffcheck <- climate_conflict_original %>% dplyr::select(country, year_actual, temp_all, temp_all_lag, temp_all_dif, prec_all, prec_all_lag, prec_all_dif)

view(diffcheck)

diffcheck <- diffcheck %>% mutate(tmp_diff = temp_all - temp_all_lag,
                                  pre_diff = prec_all - prec_all_lag)

identical(diffcheck$temp_all_dif, diffcheck$tmp_diff) # FALSE

view(diffcheck) # but they look the same

diffcheck <- diffcheck %>% mutate(check_tmp_diff = temp_all_dif - tmp_diff,
                                  check_pre_diff = prec_all_dif - pre_diff)

view(diffcheck)  # these must be rounding errors, there's a really small difference between original and mine computation
                 # also , in some cases both are exactly (!) equal

t.test(diffcheck$check_tmp_diff) # there must be some rounding error somewhere, and it's definitely not significant
t.test(diffcheck$check_pre_diff) # same here 


# let's check for heteroscedasticity real quick though !

ggplot(diffcheck, aes(temp_all, check_tmp_diff)) +
  geom_point(aes(colour = factor(country)), size = 1) # looks like a textbookexample of homoscedasticity with some outliers to me :)

ggplot(diffcheck, aes(prec_all, check_pre_diff)) +
  geom_point(aes(colour = factor(country)), size = 1) # different here ..!! we have heteroscedasticity . but why...?

diffcheck$id <- as.numeric(row.names(diffcheck))

#check for autocorrelation

ggplot(diffcheck, aes(id, check_pre_diff)) +
  geom_point(aes(colour = factor(country)), size = 1) # I do not see autocorrelation, heteroscedasticity seems more realistic
                                                      # but I don't see why rounding errors should be proportional to the value..

ggplot(diffcheck, aes(year_actual, check_pre_diff)) + 
  geom_point(aes(colour = factor(country)), size = 1) # it does seem really random


    # I will , at least for now,  anyway use this computation and assume it to be the correct one (and try not to forget it in my 
    # written examination of the reproducibilty example)

## now let's turn to the deviation from trend

  # this is gonna be  more difficult..
  # what is the trend ? 
      # in general , or (more likely), country-speficit? ## edit: country-level, noted in replication manual

      # how is this trend computed .. linear, moving average ...?


devcheck <- climate_conflict_original %>% dplyr::select(year_actual, country, temp_all, cru_temp_diftrend, prec_all, cru_prec_diftrend)

view(devcheck)

#estimate trend for tmp
trendcoef <- devcheck %>% 
  group_by(country) %>% 
  do(model_lin_tmp = lm(temp_all ~ year_actual, .)) %>%
  ungroup()

trendcoef

#use these estimates to compute predictions for all obs.

devcheck <- left_join(devcheck, trendcoef, by = "country")

devcheck <- devcheck %>% group_by(country) %>% do(modelr::add_predictions(., first(.$model_lin_tmp), var = "pred_lin_tmp"))

#difference between prediction and observed value (+ difference between my computation and original one)

devcheck <- devcheck %>% mutate(tmp_lin_dev = temp_all - pred_lin_tmp,
                                check_tmp_lin_dev = tmp_lin_dev - cru_temp_diftrend) 

#do same for pre

trendcoef <- devcheck %>% 
  group_by(country) %>%
  do(model_lin_pre = lm(prec_all ~ year_actual, .)) %>%
  ungroup()

devcheck <- left_join(devcheck, trendcoef, by = "country")

devcheck <- devcheck %>% group_by(country) %>% do(modelr::add_predictions(., first(.$model_lin_pre), var = "pred_lin_pre"))

devcheck <- devcheck %>% mutate(pre_lin_dev = prec_all - pred_lin_pre,
                                check_pre_lin_dev = pre_lin_dev - cru_prec_diftrend)

view(devcheck)

  # different ... try other model
  # weirdly enough : why is my estimate highly similar for  the obs. in year 1995 ??

# check for heteroskecasticity

ggplot(devcheck, aes(temp_all, check_tmp_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # weird shape.. some countries have pretty big deviations

ggplot(devcheck, aes(prec_all, check_pre_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # same same

ggplot(devcheck, aes(year_actual, check_tmp_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # beautiful autocorrelation.. what went wrong ?  do I need to force the model through specific points, e.g. mean?
          
      # the weirdest thing is that for all except for three countries the lines go through zero at 1995...
      # why most but not all ?!?!


ggplot(devcheck, aes(country, check_tmp_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1)

## let's try another way !!

# we should be able to compare it through another method --> creating a scatterplot from the actual observations, 
# adding the deviation and try to see which model this could be... this makes so much more sense ! 

# we try with mali first

malidev <- devcheck %>% filter(country == "Mali")
malidev <- malidev %>% mutate(originaltrend = temp_all - cru_temp_diftrend)

ggplot(malidev, aes(year_actual, originaltrend)) +
  geom_point(aes(colour = factor(country)), size = 1) + 
  stat_smooth(method = "lm", 
            formula = y ~ x)
      
      # it is actually linear !!
      

#i do my model again
malitrend <- lm(temp_all ~ year_actual, data = malidev, na.action = na.exclude)

malidev <- cbind(malidev, malitrend = fitted(malitrend))
plot(malidev$year_actual, malidev$temp_all, type = "l", col = "red")
lines(malidev$year_actual, malidev$originaltrend, col = "green")
lines(malidev$year_actual, malidev$malitrend, col = "blue")

#maybe without intercept
malitrendnobeta0 <- lm(temp_all ~ year_actual + 0, data = malidev, na.action = na.exclude)

malidev <- cbind(malidev, malitrendnobeta0 = fitted(malitrendnobeta0))
plot(malidev$year_actual, malidev$temp_all, type = "l", col = "red")
lines(malidev$year_actual, malidev$originaltrend, col = "green")
lines(malidev$year_actual, malidev$malitrend, col = "blue")

originaltrend <- lm(originaltrend ~ year_actual , data = malidev)

summary(originaltrend)

# try with other country 
angoladev <- devcheck %>% filter(country == "Angola")
angoladev <- angoladev %>% mutate(originaltrend = temp_all - cru_temp_diftrend)

ggplot(angoladev, aes(year_actual, originaltrend)) +
  geom_point(aes(colour = factor(country)), size = 1) + 
  stat_smooth(method = "lm", 
              formula = y ~ x)

#i do my model again
angolatrend <- lm(temp_all ~ year_actual, data = angoladev, na.action = na.exclude)

angoladev <- cbind(angoladev, angolatrend = fitted(angolatrend))
plot(angoladev$year_actual, angoladev$temp_all, type = "l", col = "red")
lines(angoladev$year_actual, angoladev$originaltrend, col = "green")
lines(angoladev$year_actual, angoladev$angolatrend, col = "blue")

#maybe without intercept
angolatrendnobeta0 <- lm(temp_all ~ year_actual + 0, data = angoladev, na.action = na.exclude)

angoladev <- cbind(angoladev, angolatrendnobeta0 = fitted(angolatrendnobeta0))
plot(angoladev$year_actual, angoladev$temp_all, type = "l", col = "red")
lines(angoladev$year_actual, angoladev$originaltrend, col = "green")
lines(angoladev$year_actual, angoladev$angolatrendnobeta0, col = "blue")

originaltrend <- lm(originaltrend ~ year_actual , data = angoladev)

summary(originaltrend)

## we see that for both countries , including intercept has very different effects


#do with all of them

devcheck <- devcheck %>% mutate(originaltrend_tmp = temp_all - cru_temp_diftrend,
                                originaltrend_pre = prec_all - cru_prec_diftrend) #create the linear trends

ggplot(devcheck, aes(year_actual, originaltrend_tmp)) +
  geom_point(aes(colour = factor(country)), size = 1)

ggplot(devcheck, aes(year_actual, originaltrend_pre)) +
  geom_point(aes(colour = factor(country)), size = 1)

## plot originaltrend vs. my predictions

ggplot(devcheck, aes(pred_lin_tmp, originaltrend_tmp)) +
  geom_point(aes(colour = factor(country)), size = 1)

ggplot(devcheck, aes(pred_lin_pre, originaltrend_pre)) +
  geom_point(aes(colour = factor(country)), size = 1)

# too small to see


#### this is a big mystery to me... 
#     one possible explanation, which I can't proof (without using the old CRU dataset), is , that they used more data 
#     points than are available in this replication file .
#     the CRU data spans from much earlier, and they used earlier data points ( for the lag variable), so this might explain it .
#


## check with my computed data

rm(list =ls())

mydata <- read_csv("./csv_files/climate_conflict.csv")

original <- read.dta("./climate_conflict_replication_(original)/climate_conflict.dta")


mydata <- mydata %>% left_join(original , by = c("years" = "year_actual", "countryname" = "country"))

summary(mydata$pre_difftrend)
summary(mydata$cru_prec_diftrend)

summary(mydata$tmp_difftrend)
summary(mydata$cru_temp_diftrend)

ggplot(mydata, aes(tmp_difftrend, cru_prec_diftrend)) +
  geom_point(aes(colour = factor(countryname)))

ggplot(mydata, aes(years, tmp_difftrend)) +
  geom_point(aes(colour = factor(countryname)))
