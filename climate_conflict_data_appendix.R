### generates the figures and tables in the data appendix


## setting up workspace

rm(list = ls())
setwd("C:/R/richter_climate_conflict_replication_exercise")
options(scipen = 999, digits = 2)

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
              "wbstats", "pwt","pwt9", "data.table", "foreign", "plm", "stargazer", "R.utils", "compare",
              "maptools", "lmtest", "sandwich", "mosaic", "knitr","kableExtra", "tikzDevice", "ggpubr")

package_load(packages)

# set ggplot theme

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


# load analysis data

climate_conflict <- read_csv("./analysis_data/climate_conflict.csv")

climate_conflict_alternative <- read_csv("./analysis_data/climate_conflict_alternativecountryset.csv")

climate_conflict$years <- as.character(climate_conflict$years)

## climate_conflict

# all variables

# summary descriptive  by mosaic package

climate_conflict$conflict <- as.character(climate_conflict$conflict)
climate_conflict$conflict_onset <- as.character(climate_conflict$conflict_onset) # so they get counted as categorical by inspect()

inspect_climate_conflict <- (climate_conflict %>% inspect())

# tables for quant and for categorical summaries -> will use later

cat_table_df <- as.data.frame(inspect_climate_conflict$categorical)

quant_table_df <- as.data.frame(inspect_climate_conflict$quantitative)

quant_table_df <- quant_table_df %>% select(-class)  # they are all numeric anyway, no class needed

# countries

table(climate_conflict$iso3)
uniqueN(climate_conflict$iso3)

# years

table(climate_conflict$years)
uniqueN(climate_conflict$years)

# conflict

conflict_table_df <- cat_table_df %>% filter(str_detect(name,"^conflict"))

kable(conflict_table_df, "latex", caption = "Descriptive Summary Statistics of Conflict Variables" , booktabs = T) %>% kable_styling(font_size = 9)

climate_conflict$conflict <- as.numeric(climate_conflict$conflict) # back to numeric for building the means

climate_conflict$conflict_onset <- as.numeric(climate_conflict$conflict_onset)

q1 <- ggplot(climate_conflict, aes(conflict)) +
  geom_histogram(binwidth = 0.1)


conflict_means <-  aggregate(climate_conflict$conflict, list(climate_conflict$iso3), mean) # building means of conflict observations for each country

colnames(conflict_means) <- c("iso3", "conflict_mean") # rename columns

q2 <- ggplot(conflict_means, aes(iso3,conflict_mean)) +
  geom_bar(stat = "identity") +
  ylab("conflict mean") +
  theme(axis.text.x = element_text(angle = 90))

conflict_years <- aggregate(climate_conflict$conflict, list(climate_conflict$years), sum) # summing up over years, should give nice overview

colnames(conflict_years) <- c("years", "conflict_sum")

q3 <- ggplot(conflict_years, aes(years,conflict_sum)) +
  geom_bar(stat = "identity") +
  ylab("conflict sum")

q4 <- ggplot(climate_conflict, aes(conflict_onset)) +
  geom_histogram(binwidth = 0.1) +
  xlab("conflict\\_onset")


tikz("./tikz/conflict_hist.tex", width = 4.5, height = 4.5)

conflict_hist <- ggarrange(ggarrange(q1,q4, ncol = 2),q2,q3, nrow = 3)

conflict_hist 

dev.off()

# climate

climate_table_df <- quant_table_df %>% filter(str_detect(name, "^tmp") | str_detect(name, "^pre"))

kable(climate_table_df, "latex", caption = "Descriptive Summary Statistics of Climate Variables" , booktabs = T) %>% kable_styling(font_size = 9)


# tmp

climate_conflict_tmp <- climate_conflict %>% select(starts_with("tmp")) %>% select(-contains("lag")) # select tmp variables, excluding lag

variable_names_tmp <- list(
  "tmp" = "tmp" ,
  "tmp_lead" = "tmp\\_lead",
  "tmp_square" = "tmp\\_square",
  "tmp_diff" = "tmp\\_diff",
  "tmp_difftrend" =   "tmp\\_difftrend",
  "tmp_wrld_simpl" = "tmp\\_wrld\\_simpl"
)

variable_labeller_tmp <- function(variable,value){
  return(variable_names_tmp[value])
}

tikz("./tikz/tmp_hist.tex", width = 4.5, height = 3.5)

tmp_hist <- ggplot(gather(climate_conflict_tmp, factor_key = T), aes(value)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  facet_wrap(~key, scales = 'free', labeller=variable_labeller_tmp, ncol =2)

tmp_hist

dev.off()

# pre

climate_conflict_pre <- climate_conflict %>% select(starts_with("pre")) %>% select(-contains("lag"))

variable_names_pre <- list(
  "pre" = "pre" ,
  "pre_lead" = "pre\\_lead",
  "pre_square" = "pre\\_square",
  "pre_diff" = "pre\\_diff",
  "pre_difftrend" =   "pre\\_difftrend",
  "pre_wrld_simpl" = "pre\\_wrld\\_simpl"
)

variable_labeller_pre <- function(variable,value){
  return(variable_names_pre[value])
}

tikz("./tikz/pre_hist.tex", width = 4.5, height = 3.5)

pre_hist <- ggplot(gather(climate_conflict_pre, factor_key = T), aes(value)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  facet_wrap(~key, scales = 'free', labeller=variable_labeller_pre, ncol =2)

pre_hist

dev.off()

# control

control_table_df <- quant_table_df %>% filter(str_detect(name, "^gdp") | str_detect(name, "^polity")) %>% arrange(name)

control_table_df <- control_table_df[c(1,2,3,4,5,8,6,7),] # quick and dirty manual reorder to have same nice together

rownames(control_table_df) <- c() # get rid of rownames

kable(control_table_df, "latex", caption = "Descriptive Summary Statistics of Control Variables" , booktabs = T) %>% kable_styling(font_size = 9)


# gdp

tikz("./tikz/gdp_hist.tex", width = 4.5, height = 3.5)

gdp_hist <- ggplot(climate_conflict, aes(gdp)) +
  geom_histogram(binwidth = 0.1)

gdp_pwt9_hist <- ggplot(climate_conflict, aes(gdp_pwt9)) +
  geom_histogram(binwidth = 0.1) +
  xlab("gdp\\_pwt9")

ggarrange(gdp_hist, gdp_pwt9_hist, nrow = 2)
dev.off()

# polity 

tikz("./tikz/polity_hist.tex", width = 3.5, height = 3.5)

polity_hist <- ggplot(climate_conflict, aes(polity2)) +
  geom_histogram(binwidth = 0.1)
polity2018_hist <- ggplot(climate_conflict, aes(polity2_2018)) +
  geom_histogram(binwidth = 0.1) +
  xlab("polity2\\_2018")

ggarrange(polity_hist, polity2018_hist, nrow = 2)
dev.off()


## now for alternative countryset file

# all variables

# summary descriptive  by mosaic package

climate_conflict_alternative$conflict <- as.character(climate_conflict_alternative$conflict)
climate_conflict_alternative$conflict_onset <- as.character(climate_conflict_alternative$conflict_onset) # so they get counted as categorical by inspect()

inspect_climate_conflict_alternative <- (climate_conflict_alternative %>% inspect())

# tables for quant and for categorical summaries -> will use later

cat_table_df <- as.data.frame(inspect_climate_conflict_alternative$categorical)

quant_table_df <- as.data.frame(inspect_climate_conflict_alternative$quantitative)

quant_table_df <- quant_table_df %>% select(-class)  # they are all numeric anyway, no class needed

# countries

table(climate_conflict_alternative$iso3)
uniqueN(climate_conflict_alternative$iso3)

# years

table(climate_conflict_alternative$years)
uniqueN(climate_conflict_alternative$years)

# conflict

conflict_table_df <- cat_table_df %>% filter(str_detect(name,"^conflict"))

kable(conflict_table_df, "latex", caption = "Descriptive Summary Statistics of Conflict Variables in alternative country dataset" , booktabs = T) %>% kable_styling(font_size = 9)

climate_conflict_alternative$conflict <- as.numeric(climate_conflict_alternative$conflict) # back to numeric for building the means

climate_conflict_alternative$conflict_onset <- as.numeric(climate_conflict_alternative$conflict_onset)

q1 <- ggplot(climate_conflict_alternative, aes(conflict)) +
  geom_histogram(binwidth = 0.1)


conflict_means <-  aggregate(climate_conflict_alternative$conflict, list(climate_conflict_alternative$iso3), mean) # building means of conflict observations for each country

colnames(conflict_means) <- c("iso3", "conflict_mean") # rename columns

q2 <- ggplot(conflict_means, aes(iso3,conflict_mean)) +
  geom_bar(stat = "identity") +
  ylab("conflict mean") +
  theme(axis.text.x = element_text(angle = 90))

conflict_years <- aggregate(climate_conflict_alternative$conflict, list(climate_conflict_alternative$years), sum) # summing up over years, should give nice overview

colnames(conflict_years) <- c("years", "conflict_sum")

q3 <- ggplot(conflict_years, aes(years,conflict_sum)) +
  geom_bar(stat = "identity") +
  ylab("conflict sum")

q4 <- ggplot(climate_conflict_alternative, aes(conflict_onset)) +
  geom_histogram(binwidth = 0.1) +
  xlab("conflict\\_onset")


tikz("./tikz/conflict_hist_alt.tex", width = 4.5, height = 4.5)

conflict_hist_alt <- ggarrange(ggarrange(q1,q4, ncol = 2),q2,q3, nrow = 3)

conflict_hist_alt

dev.off()

# climate

climate_table_df <- quant_table_df %>% filter(str_detect(name, "^tmp") | str_detect(name, "^pre"))

kable(climate_table_df, "latex", caption = "Descriptive Summary Statistics of Climate Variables in alternative countryset" , booktabs = T) %>% kable_styling(font_size = 9)


climate_conflict_alternative_climate_alt <- climate_conflict_alternative %>% select(matches("tmp|pre")) %>% select(-contains("lag")) # select tmp variables, excluding lag

variable_names_climate_alt <- list(
  "tmp_alt_countries" = "tmp\\_alt\\_countries" ,
  "tmp_alt_countries_diff" = "tmp\\_alt\\_countries\\_diff",
  "pre_alt_countries" = "pre\\_alt\\_countries" ,
  "pre_alt_countries_diff" = "pre\\_alt\\_countries\\_diff"
)

variable_labeller_climate_alt <- function(variable,value){
  return(variable_names_climate_alt[value])
}

tikz("./tikz/climate_hist_alt.tex", width = 4.5, height = 3.5)

climate_hist_alt <- ggplot(gather(climate_conflict_alternative_climate_alt, factor_key = T), aes(value)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  facet_wrap(~key, scales = 'free', labeller=variable_labeller_climate_alt, ncol =2)

climate_hist_alt

dev.off()

# control

control_table_df <- quant_table_df %>% filter(str_detect(name, "^gdp") | str_detect(name, "^polity")) %>% arrange(name)

rownames(control_table_df) <- c() # get rid of rownames

kable(control_table_df, "latex", caption = "Descriptive Summary Statistics of Control Variables" , booktabs = T) %>% kable_styling(font_size = 9)


tikz("./tikz/control_hist_alt.tex", width = 4.5, height = 3.5)

gdp_hist_alt <- ggplot(climate_conflict_alternative, aes(gdp)) +
  geom_histogram(binwidth = 0.1)

polity_hist_alt <- ggplot(climate_conflict_alternative, aes(polity2)) +
  geom_histogram(binwidth = 0.1)

ggarrange(gdp_hist_alt, polity_hist_alt, nrow = 2)
dev.off()
