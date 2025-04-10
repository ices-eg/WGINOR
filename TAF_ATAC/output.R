## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(tidyverse)

mkdir("output")

#load data
data <- read.taf("data") #load data (list of the different data tables)
series <- as_tibble(data$series)
info <- as_tibble(data$info)

table.all=list()
info$AR <- NA
for (ycol in c(2:dim(series)[2])){ # loop on all time-series 
  load(file.path(".","model",paste0("modelfits",ycol,".Rdata")))
  print(ycol)
  results <- try(ATAC.fit$results,silent=TRUE) 
  if (info$Transformation[ycol]==TRUE) results$value <- try(results$value^4,TRUE)
  table.all[[ycol]] <- results
  info$AR[ycol] <- try(ATAC.fit$AR,silent=TRUE)
}
names(table.all)<-info$ID
save(table.all, info, file = "./output/tables.Rdata")
