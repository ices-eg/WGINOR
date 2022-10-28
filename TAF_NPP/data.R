## Preprocess data, write TAF data tables

# the script downloads the VGPM primary production model outputs from the Oregon State University server, for 8-day periods,
# and archives the data for the Northeast Atlantic region, in grid format.
# In addition, it constructs the NPPseries.txt file that contains the filenames and dates for all the downloaded files.


# libraries and directory --------------------------------------------
library(icesTAF)
require(tidyverse) # to wrangle data
require(R.utils) # to unzip .gz files
library(raster) #To read and manipulate raster data

setwd("./TAF_NPP")
mkdir("data")

# initialisation ----------------------------------------------------------
h42h5 <- FALSE # Switch to indicate whether the translation from HDF4 to HDF5 format is needed
if (str_detect(sessionInfo()$platform,pattern = "darwin")){ # check if the script is run from a Mac
  if (file.exists(file.path(".","h42h5"))==FALSE){ # check if the h42h5 program exists
    download.file(url="https://gamma.hdfgroup.org/ftp/pub/outgoing/h4h5tools/h4toh5bin/MacOS1015/h4toh5",destfile = "h4toh5") # if not, download it
    system("chmod u+x h4toh5") # and install it
  }
  h42h5 <- TRUE # switch to specify if the original hdf files (in H4 format) need to be transformed into H5 (typically, this is set to TRUE on Mac and FALSE on Windows)
}

# Download and reformat the NPP data from Oregon State Univ.
url_path <- "http://orca.science.oregonstate.edu/data/1x2/8day/vgpm.r2018.m.chl.m.sst/hdf/"
PageInfo<-readLines(url_path) # read the OSU webpage that list all the 8-day files files available
filenames <- substr(PageInfo[which(nchar(PageInfo)==225)],85,96) # extract names without the extension .hdf.gz
#Create a table that document filenames and corresponding date.  
NPP.series<-tibble(filename=filenames,
                   date=NA,
                   year=as.integer(substr(filenames,6,9)),
                   doy=as.integer(substr(filenames,10,12)))
NPP.series$date<-as.Date(paste(NPP.series$year,"-01-01",sep=""))+as.numeric(NPP.series$doy) # get back to a date format
write.csv(NPP.series,file=file.path(".","data","NPPseries.txt"),row.names = FALSE)

# geographical boundaries ------------------------------------
# construct a polygon (in the sp format) with the geographical limits within which NPP data are extracted 
poly<-matrix(c(-40,-40,70,70,-40,48,80,80,48,48),ncol=2)
# poly<-list(poly)# to be understood as a polygon, the object has to be a list and for each polygon the first and last rows needs to be the same ()
poly <- sp::Polygon(list(poly))
Poly <- sp::SpatialPolygons(list(sp::Polygons(list(poly),ID = "A")))


# download and preprocess VGPM data ---------------------------------------

for (filename in filenames){ # loop on all individual files
  if (file.exists(file.path(".","data",paste0(filename,"crop.gri")))==FALSE){ # test if the file has already been processed
    download.file(url=paste0(url_path,filename,".hdf.gz"),paste0("./",filename,".hdf.gz")) # download individual files
    gunzip(paste0("./",filename,".hdf.gz"), remove=TRUE, overwrite = TRUE)
    if(h42h5==TRUE){ # transform from hdf4 to hdf5, if needed (on Mac/Linux)
      system(paste0("./h4toh5 ",filename,".hdf")) # convert from hdf to h5 format
      system(paste0("mv -f ",filename,".h5 ",filename,".hdf"))
    }
    NPP<-raster::stack(paste0(filename,".hdf")) # load as raster
    NAvalue(NPP) <- -999 # transform -999 in NAs
    NPP <- setExtent(NPP, extent(-180, 180, -90, 90)) # specify the current spatial extension of the vgpm files
    crs(NPP) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    NPPcrop <- raster::crop(NPP,Poly) # crop for the selected geographical domain
    writeRaster(NPPcrop,file.path(".","data",paste0(filename,"crop"))) # save the cropped data in "gri"/raster format
    system(paste0("rm ",filename,".hdf")) # remove the original hdf file
  }
}
