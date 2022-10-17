## Run analysis, write model results

# This scripts takes as input the NPP outputs (8-day periods) for the Northeast Atlantic as .gri files
# It also takes as input the polygons used by WGINOR
# It extract the NPP estimates within the polygon for 8-day periods and calculates the summed annual NPP (over the period for which records are available)
# It also fits a hierarchical seasonal model of NPP and uses the results to provide the following
# - daily and 8-day NPP estimates
# - Annual integrated NPP
# - time of peak NPP
# - time of NPP-season beginning
# - time of NPP-season end

# libraries --------------------------------------------
library(icesTAF)
library(raster)
library(readxl)
library(sf)
require(tidyverse) # to wrangle data

mkdir("model")


# initialisation ----------------------------------------------------------
resampling <- TRUE # TRUE if the confidence intervals are estimated with boostrapping, FALSE if CI are parametric estimates 
nboot <- 1000 # number of resampling (bootstrap)
polygon_name <- "NPP2022norw" # Name of the polygon to extract data from
NPP.series <- read_csv(file=file.path(".","data","NPPseries.txt")) # list of the NPP data files
NPP.series <- NPP.series %>% filter(year>2002) %>% # remove 2002 (incomplete data)
  mutate(NPP.mean=NA,NPP.025=NA,NPP.975=NA)


# read polygons data file --------------------------------------------------
polygonfile <- file.path(".","bootstrap","data","WGINORStrata.xlsx")
for(sheet in excel_sheets(polygonfile)){
  eval(parse(text=paste0(sheet," = read_xlsx(sheet='",sheet,"','",polygonfile,"')")))
}
Polygons <- left_join(Coordinates,Strata,by="StrataKey") %>%
  left_join(StrataSystem,by="StrataSystemKey")
Polygons_sf <- purrr::map(1:dim(Polygons)[1],function(i){
  st_as_sfc(Polygons$Coordinates[i], crs = 4326) # same as crs=CRS("+proj=longlat +datum=WGS84")) using library rgdal
})
names(Polygons_sf) <- Polygons$StrataKey

# format polygon for NPP extraction ---------------------------------------
Poly=sf:::as_Spatial(Polygons_sf[names(Polygons_sf)==polygon_name][[1]]) # get the polygon in "spatial polygons" format

# compute NPP for the selected polygon for 8d periods ---------------------------------
NPP.resampled<-list()

for (i in 1:dim(NPP.series)[1]){
  NPP <- raster::stack(file.path(".","data",paste0(NPP.series$filename[i],"crop.gri")))
  NAvalue(NPP) <- -999 # transform -999 in NAs
  NPP.in.poly <- raster::extract(NPP,Poly,extract=TRUE)[[1]] # extract a vector with all observations in the polygon for one date
  nobs <- sum(!is.na(NPP.in.poly)) # number of valid observations
  print(nobs)
  if (nobs>dim(NPP.in.poly)[1]/2){ # check that there are observations for at least 1/2 the polygon
    if (resampling==FALSE){
      se <- sd(NPP.in.poly,na.rm=TRUE)/(nobs)^0.5
      NPP.series$NPP.mean[i] <- mean(NPP.in.poly,na.rm=TRUE)
      NPP.series$NPP.025[i] <- NPP.series$NPP.mean[i]-1.96*se
      NPP.series$NPP.975[i] <- NPP.series$NPP.mean[i]+1.96*se
    } else {
      NPP.resampled[[i]] <- purrr::map_df(1:nboot,function(i){
        X<-sample(NPP.in.poly,replace = TRUE)
        out <-tibble(meanNPP=mean(X,na.rm=TRUE))
        return(out)
      })
      NPP.series$NPP.mean[i] <- quantile(NPP.resampled[[i]]$meanNPP,0.5)
      NPP.series$NPP.025[i] <- quantile(NPP.resampled[[i]]$meanNPP,0.025)
      NPP.series$NPP.975[i] <- quantile(NPP.resampled[[i]]$meanNPP,0.975)
    }
  }
}

# compute NPP for the selected polygon - annual estimates ---------------------------------
NPP.annual <- tibble(year = unique(NPP.series$year),NPP.mean=NA,NPP.025=NA,NPP.975=NA)
if (resampling==TRUE){
  NPP.annual[,2:4] <- purrr::map_df(NPP.annual$year, function(year){
    w <- which((NPP.series$year==year)&(!is.na(NPP.series$NPP.mean))) # identify valid individual files within a year
    NPPmat <- do.call(cbind,NPP.resampled[w]) # construct a matrix of resampled means with nboot lines
    out <-quantile(apply(NPPmat,1,sum)*8/1000,c(0.5,0.025,0.975))
  })
}




# need to add seasonal models and corresponding annual estimates and phenological estimates
