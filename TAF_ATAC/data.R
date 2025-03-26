## Preprocess data, write TAF data tables
# Comment line added for test purpose during the WGINOR 2022 meeting

require(icesTAF)
require(readxl)
require(tidyverse)

mkdir("data")
datafile <- taf.data.path("WGINOR_Dataset_2024.xlsx")

sheets <- excel_sheets(datafile) # read the data from the "standard" TAF data path (~/bootstrap/data/tree.csv)
info <- read_xlsx(taf.data.path("WGINOR_Dataset_2024.xlsx"),skip=3,sheets[1]) %>%
  rename(ID=`ShortName (no special characters, no space)`,
         FullName=`Variable name`,
         LastUpdate=`last update`,
         Link=`Ressource link, doi???`,
         Contact=`Contact person`,
         Email=email,
         Category=ATACCategory) %>%
  select(ID,FullName,Unit,LastUpdate,Link,Source,Description,Contact,Email,Category,ATAC) %>%
  filter(ATAC!=0) %>%
  mutate(transformation = case_when(
    ATAC == 1 ~ FALSE,
    ATAC == 2 ~ TRUE)) %>%
  select(-ATAC) %>%
  mutate_all(as.character) %>%
  mutate(transformation=as.logical(transformation)) %>%
  add_row(ID="Year",FullName="Year",transformation=FALSE,.before=1)
series <- read_xlsx(taf.data.path("WGINOR_Dataset_2024.xlsx"),sheets[2]) %>%
  select(info$ID) %>%
  mutate_all(as.numeric)

series[,info$transformation] <- series[,info$transformation]^0.25 # transform selected data series using double square root (c(FALSE) is to account for the "Year" column)
write.taf(series, dir = "data", file = "series.csv") # write the data series 
write.taf(info, dir = "data", file = "info.csv", quote=TRUE) # write the meta-data
