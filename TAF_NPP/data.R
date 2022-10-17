## Preprocess data, write TAF data tables

# the script downloads the VGPM primary production model outputs from the Oregon State University server, for 8-day periods,
# and archives the data for the Northeast Atlantic region, in grid format.
# In addition, it constructs the NPPseries.txt file that contains the filenames and dates for all the downloaded files.


# libraries --------------------------------------------
library(icesTAF)
library(raster)
require(tidyverse) # to wrangle data

mkdir("data")

# initialisation ----------------------------------------------------------
h42h5 <- TRUE # switch to specify if the original hdf files (in H4 format) need to be transformed into H5 (typically, this is set to TRUE on Mac and FALSE on Windows)

# Download and reformat the NPP data from Oregon State Univ.
url_path <- "http://orca.science.oregonstate.edu/data/1x2/8day/vgpm.r2018.m.chl.m.sst/hdf/"
filelist<-read.taf(taf.data.path("vgpmfilenames.txt"),sep="\t",header=FALSE) # list of all the 8-day files from the OSU server
filenames <- substr(filelist$V2,1,12) # extract names without the extension .hdf.gz
NPP.series<-tibble(filename=filenames,
                   date=NA,
                   year=as.integer(substr(filelist$V2,6,9)),
                   doy=as.integer(substr(filelist$V2,10,12)))
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
    system(paste0("gzip -d ",filename,".hdf")) # unzip
    if(h42h5==TRUE){ # transform from hdf4 to hdf5, if needed
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

