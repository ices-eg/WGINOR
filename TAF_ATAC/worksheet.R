# TAF_ATACH_WORKSHEET

require(icesTAF)

taf.skeleton(".") #<- Create initial directories and R scripts for a new TAF analysis.
data.path <- taf.boot.path("initial","data") # <- points towards the original datafiles

draft.data( # create an entry in data.bib (this will be implemented when running taf.bootstrap)
  data.files = "NOSdata_VariablesInfo.csv",
  data.scripts =NULL,
  originator = "Lucie Buttay",
  title = "Description of each variable for the time series used in WGINOR in 2021",
  file = TRUE,
  append = FALSE
)

draft.data( # create an entry in data.bib (this will be implemented when running taf.bootstrap)
  data.files = "NOSdata_TREC_20220603.txt",
  data.scripts =NULL,
  originator = "Lucie Buttay",
  title = "Time series used in WGINOR in 2021",
  file = TRUE,
  append = TRUE
)
taf.bootstrap() #<- Process metadata files ???SOFTWARE.bib??? and ???DATA.bib??? to set up software and data files required for the analysis.

