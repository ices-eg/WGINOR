## Preprocess data, write TAF data tables
# Comment line added for test purpose during the WGINOR 2022 meeting

library(icesTAF)

mkdir("data")

series <- read.taf(taf.data.path("NOSdata_2023.11.13.csv")) # read the data from the "standard" TAF data path (~/bootstrap/data/tree.csv)
colnames(series)[1] <- "Year" # force the first column to be named "Year"
info <- read.taf(taf.data.path("NOSdata_VariablesInfo_2023.11.13.csv")) # read the data from the "standard" TAF data path (~/bootstrap/data/tree.csv)
series[,info$transformation] <- series[,info$transformation]^0.25 # transform selected data series using double square root
write.taf(series, dir = "data", file = "series.csv") # write the data series 
write.taf(info, dir = "data", file = "info.csv") # write the meta-data


