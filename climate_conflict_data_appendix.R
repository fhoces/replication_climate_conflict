### generates the figures and tables in the data appendix


## setting up workspace

rm(list = ls())
setwd("C:/R/bachelorproject")
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
              "maptools", "lmtest", "sandwich", "mosaic", "knitr","kableExtra", "tikzDevice")

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

# climate

climate_table_df <- quant_table_df %>% filter(str_detect(name, "^tmp") | str_detect(name, "^pre"))

kable(climate_table_df, "latex", caption = "Descriptive Summary Statistics of Climate Variables" , booktabs = T) %>% kable_styling(font_size = 9)

# conflict

climate_conflict$conflict <- as.numeric(climate_conflict$conflict) # back to numeric for building the means

conflict_means <-  aggregate(climate_conflict$conflict, list(climate_conflict$iso3), mean) # building means of conflict observations for each country

colnames(conflict_means) <- c("iso3", "conflict_mean") # rename columns

ggplot(conflict_means, aes(iso3,conflict_mean, fill = iso3)) +
  geom_bar(stat = "identity")


conflict_years <- aggregate(climate_conflict$conflict, list(climate_conflict$years), sum) # summing up over years, should give nice overview

colnames(conflict_years) <- c("years", "conflict_sum")

ggplot(conflict_years, aes(years,conflict_sum, fill = years)) +
  geom_bar(stat = "identity")

# gdp 




climate_conflict_tmp <- climate_conflict %>% select(starts_with("tmp"))
climate_conflict_pre <- climate_conflict %>% select(starts_with("pre"))
climate_conflict_polity <- climate_conflict %>% select(starts_with("polity"))
climate_conflict_conflict <- climate_conflict %>% select(starts_with("conflict"))

variable_names <- list(
  "tmp" = "tmp" ,
  "tmp_lag" = "tmp\\_lag",
  "tmp_lead" = "tmp\\_lead",
  "tmp_square" = "tmp\\_square",
  "tmp_lag_square" = "tmp\\_lag\\_square",
  "tmp_diff" = "tmp\\_diff",
  "tmp_diff_lag" =   "tmp\\_diff\\_lag",
  "tmp_difftrend" =   "tmp\\_difftrend",
  "tmp_difftrend_lag" =   "tmp\\_difftrend\\_lag",
  "tmp_wrld_simpl" = "tmp\\_wrld\\_simpl",
  "tmp_wrld_simpl_lag" = "tmp\\_wrld\\_simpl\\_lag"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

tikz("./tikz/tmp_hist.tex", width = 4, height = 4)

tmp_hist <- ggplot(gather(climate_conflict_tmp, factor_key = T), aes(value)) +
    geom_histogram(binwidth = 0.1, colour = "black") +
    facet_wrap(~key, scales = 'free', labeller=variable_labeller, ncol =2)

tmp_hist

dev.off()

# tmp_lag

kable(favstats(climate_conflict$tmp_lag), "latex", caption = "tmp\\_lag summary statistics", booktabs = T) %>% kable_styling(font_size = 9)

# tmp_lead

kable(favstats(climate_conflict$tmp_lead), "latex", caption = "tmp\\_lead summary statistics", booktabs = T) %>% kable_styling(font_size = 9)

# gdp 

summaryFull(climate_conflict$gdp)[2]
