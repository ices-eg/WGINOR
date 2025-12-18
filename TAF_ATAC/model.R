## Run analysis, write model results

## Before:
## After:

require(icesTAF)
require(ggplot2)
require(tidyverse)
require(mgcv)
require(brms)
sourceTAF("utilities")

mkdir("model")

# load data
data <- read.taf("data") # load data (list of the different data tables)
series <- as_tibble(data$series)
info <- as_tibble(data$info)

# set up model specifications
year_start <- 1980 # starting year for the graphs
year_end_OL <- 2022 # ending year for the fitting
n_forecasts <- 3 # number of years to forecast

# precompile brms models to speed up the modelling
inputseries <- series %>%
  transmute(x = Year, y = .[, sample(1:dim(series)[2], size = 1)][[1]]) %>%
  filter(is.na(y) == 0) %>%
  as_tibble() # select a random time series to prefit the models
prefits <- preATAC(inputseries,
  year_start = year_start,
  year_end_OL = year_end_OL,
  n_forecasts = n_forecasts
)
# run model
for (ycol in c(2:dim(series)[2])) { # loop on all time-series
  inputseries <- series %>%
    transmute(x = Year, y = .[, ycol][[1]]) %>%
    filter(is.na(y) == 0) %>%
    as_tibble() # wrangle data prior to modelling
  ATAC.fit <- try(
    ATAC(
      prefits = prefits,
      data = inputseries,
      year_start = year_start,
      year_end_OL = year_end_OL,
      n_forecasts = n_forecasts
    ),
    TRUE
  ) # fit models to an individual time series
  save(ATAC.fit, file = file.path(".", "model", paste0("modelfits", ycol, ".Rdata"))) # save each individual model
}
