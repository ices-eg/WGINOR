# Worksheet script - initialise TAF directories, etc.
library(icesTAF)
taf.skeleton()
data.path <- taf.boot.path("initial","data") # points to the "raw2 original data files

draft.data(data.files = "vgpmfilenames.txt",
           data.scripts = NULL,
           originator = "Lucie Buttay and Benjamin Planque",
           title = "list of NPP-VGPM files located on the Oregon State University server",
           source = "http://orca.science.oregonstate.edu/data/1x2/8day/vgpm.r2018.m.chl.m.sst/hdf",
           period = "2002-2021",
           file = TRUE,
           append = FALSE,
           access = "Public")

draft.data(data.files = "WGINORStrata.xlsx",
           data.scripts = NULL,
           originator = "Benjamin Planque",
           title = "Polygons used in WGINOR in WKT format",
           file = TRUE,
           append = TRUE,
           access = "Public")

