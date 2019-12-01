### generates the figures and tables in the data appendix


## setting up workspace

rm(list = ls())
setwd("C:/R/bachelorproject")
options(scipen = 999)

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

# inspected df by mosaic package

inspect_climate_conflict <- (climate_conflict %>% inspect())

# tables for quant and for categorical

cat_table_df <- as.data.frame(inspect_climate_conflict$categorical) %>% select(-distribution)

quant_table_df <- as.data.frame(inspect_climate_conflict$quantitative)

kable(quant_table_df, "latex", caption = "descriptive summary statistics of quantitative variables" , booktabs = T) %>% kable_styling(font_size = 9)

# countries

table(climate_conflict$iso3)
uniqueN(climate_conflict$iso3)

# years

table(climate_conflict$years)
uniqueN(climate_conflict$years)

# 
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
