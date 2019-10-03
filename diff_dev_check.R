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

diffcheck <- climate_conflict_original %>% select(country, year_actual, temp_all, temp_all_lag, temp_all_dif, prec_all, prec_all_lag, prec_all_dif)

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


devcheck <- climate_conflict_original %>% select(year_actual, country, temp_all, cru_temp_diftrend, prec_all, cru_prec_diftrend)

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

  # the difference is pretty small.. I'm not sure if they used a differenct computation ... maybe try other model
  # weirdly enough : why is my estimate highly similar for all the obs. in year 1995 ??

# check for heteroskecasticity

ggplot(devcheck, aes(temp_all, check_tmp_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # weird shape.. some countries have pretty big deviations

ggplot(devcheck, aes(prec_all, check_pre_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # same same

ggplot(devcheck, aes(year_actual, check_tmp_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # beautiful autocorrelation.. what went wrong ? 
          
      # we see the center point at 1995.. what am I missing..


ggplot(devcheck, aes(country, check_tmp_lin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1)

# we should be able to compare it through another method --> creating a scatterplot from the actual observations 

# we try with mali first

malidev <- devcheck %>% filter(country == "Mali")
malidev <- malidev %>% mutate(originaltrend = temp_all - cru_temp_diftrend)

ggplot(malidev, aes(y = year_actual, x1 = originaltrend, x2 = temp_all)) +
  geom_point(aes(colour = factor(country)), size = 1) + 
  geom_smooth(aes(x = temp_all), method = "lm", formula = x ~ y)


# it is actually linear !!
#and and my simple linear regression looks exactly right ... why did I get different outcome before?

#do with all of them

devcheck <- devcheck %>% mutate(originaltrend = temp_all - cru_temp_diftrend) #create the linear trends

ggplot(devcheck, aes(year_actual, originaltrend)) +
  geom_point(aes(colour = factor(country)), size = 1)



#let's try a log-linear trend model

#again : estimate trend for tmp
trendcoef <- devcheck %>% 
  group_by(country) %>% 
  do(model_loglin_tmp = lm(log(temp_all) ~ year_actual, .)) %>%
  ungroup()

trendcoef

#use these estimates to compute predictions for all obs.

devcheck <- left_join(devcheck, trendcoef, by = "country")

devcheck <- devcheck %>% group_by(country) %>% do(modelr::add_predictions(., first(.$model_loglin_tmp), var = "pred_loglin_tmp"))

#difference between prediction and observed value (+ difference between my computation and original on)

devcheck <- devcheck %>% mutate(tmp_loglin_dev = temp_all - pred_loglin_tmp,
                                check_tmp_loglin_dev = tmp_loglin_dev - cru_temp_diftrend) 

#do same for pre

trendcoef <- devcheck %>% 
  group_by(country) %>%
  do(model_loglin_pre = lm(log(prec_all) ~ year_actual, .)) %>%
  ungroup()

devcheck <- left_join(devcheck, trendcoef, by = "country")

devcheck <- devcheck %>% group_by(country) %>% do(modelr::add_predictions(., first(.$model_loglin_pre), var = "pred_loglin_pre"))

devcheck <- devcheck %>% mutate(pre_loglin_dev = prec_all - pred_loglin_pre,
                                check_pre_loglin_dev = pre_loglin_dev - cru_prec_diftrend)

view(devcheck) #that made it worse ...

ggplot(devcheck,  aes(year_actual, check_tmp_loglin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # no more heteroscedasticity (well, really small at least) !!

  # on the bad side.. big deviations , which seem to really rely on countries
  # they're all bigger..meaning my estimates are higher . why's that ? 

ggplot(devcheck,  aes(year_actual, check_pre_loglin_dev)) +
  geom_point(aes(colour = factor(country)), size = 1) # here we still have heteroscedasticity in some countries

  #also my estimates too high

ggplot(devcheck, aes(temp_all, check_tmp_loglin_dev)) +
  geom_point(aes(colour = factor(country)), size= 1) # temperature has no influence, only countries!!

ggplot(devcheck, aes(country, check_tmp_loglin_dev)) +
  geom_point(aes(colour = factor(year_actual)), size = 1) # all the popsicles look similar ! 

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
      #and and my simple linear regression looks exactly right ... why did I get different outcome before?

#do with all of them

devcheck <- devcheck %>% mutate(originaltrend = temp_all - cru_temp_diftrend) #create the linear trends

ggplot(devcheck, aes(year_actual, originaltrend)) +
  geom_point(aes(colour = factor(country)), size = 1)


