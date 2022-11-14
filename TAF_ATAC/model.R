## Run analysis, write model results

## Before:
## After:

library(icesTAF)
require(ggplot2)
require(tidyverse)
require(mgcv)
require(brms)
sourceTAF("utilities")

mkdir("model")

#load data
data <- read.taf("data") #load data (list of the different data tables)
series <- as_tibble(data$series)
info <- as_tibble(data$info)

# run model
for (ycol in c(2:dim(series)[2])){ # loop on all time-series 
  inputseries <- series %>% transmute(x=Year,y=.[,ycol][[1]]) %>% filter(is.na(y)==0) %>% as_tibble() # wrangle data prior to modelling
  ATAC.fit <- try(ATAC(inputseries,year_start=1980),TRUE) # fit models to an individual time series
  save(ATAC.fit, file = file.path(".","model",paste0("modelfits",ycol,".Rdata"))) # save each individual model 
}
