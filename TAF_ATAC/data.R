## Preprocess data, write TAF data tables
# Comment line added for test purpose during the WGINOR 2022 meeting

require(icesTAF)
require(readxl)
require(tidyverse)

mkdir("data")
datafile <- taf.data.path("WGINOR_Dataset_2025.xlsx")

sheets <- excel_sheets(datafile) # read the data from the "standard" TAF data path (~/bootstrap/data/tree.csv)
info <- read_xlsx(datafile, skip = 3, sheets[2]) %>%
  # select(ShortNameKey, FullName, Unit, LastUpdate, Source, Citation, Description, ContactPerson, Email, ATACCategory, ATAC) %>%
  filter(ATAC != 0) %>%
  mutate(Transformation = case_when(
    ATAC == 1 ~ FALSE,
    ATAC == 2 ~ TRUE
  )) %>%
  select(-ATAC) %>%
  mutate_all(as.character) %>%
  mutate(Transformation = as.logical(Transformation)) %>%
  add_row(ShortNameKey = "Year", FullName = "Year", Transformation = FALSE, .before = 1)
series <- read_xlsx(datafile, sheets[3]) %>%
  select(info$ShortNameKey) %>%
  mutate_all(as.numeric)

series[, info$Transformation] <- series[, info$Transformation]^0.25 # transform selected data series using double square root (c(FALSE) is to account for the "Year" column)
write.taf(series, dir = "data", file = "series.csv") # write the data series
write.taf(info, dir = "data", file = "info.csv", quote = TRUE) # write the meta-data
