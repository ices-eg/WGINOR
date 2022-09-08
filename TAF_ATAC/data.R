## Preprocess data, write TAF data tables

library(icesTAF)

mkdir("data")

series <- read.taf(taf.data.path("NOSdata_TREC_20220603.txt"),sep="\t") # read the data from the "standard" TAF data path (~/bootstrap/data/tree.csv)
colnames(series)[1] <- "Year" # force the first column to be named "Year"
info <- read.taf(taf.data.path("NOSdata_VariablesInfo.csv")) # read the data from the "standard" TAF data path (~/bootstrap/data/tree.csv)
series[,info$transformation] <- series[,info$transformation]^0.25 # transform selected data series using double square root
write.taf(series, dir = "data", file = "series.csv") # write the data series 
write.taf(info, dir = "data", file = "info.csv") # write the meta-data


