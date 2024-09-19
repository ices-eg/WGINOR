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
library(readxl) # xlsx files
library(raster) # spatial data
library(sf) # spatial data
require(tidyverse) # wrangle data
require(nlme) # hierarchical models
sourceTAF("utilities")

mkdir("model")

# initialisation ----------------------------------------------------------
resampling <- FALSE # TRUE if the confidence intervals are estimated with boostrapping, FALSE if CI are parametric estimates 
nboot <- 1000 # number of resampling (bootstrap)
# polygon_name <- "NPP2022norw" # Name of the polygon to extract data from
polygon_name <- c("WGINORNorSeaE","WGINORNorSeaW","WGINORLBE","WGINORLBW") # Name of the polygon to extract data from
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
Poly <- map(polygon_name,function(pn){
  Pol <- sf:::as_Spatial(Polygons_sf[names(Polygons_sf)%in%pn][[1]]) # get the polygon in "spatial polygons" format
})

# plot map with NPP and polygon for the report ----------------------------
filenumber <- 120 # Open one vgpm croped files as an example:
NPP<-raster::stack(file.path(".","data",paste0(NPP.series$filename[filenumber],"crop.gri")))
NAvalue(NPP) <- -999 # transform -999 in NAs
crs(NPP) <- "+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
NPPdf <- as.data.frame(NPP, xy=TRUE) #Convert raster to data.frame
names(NPPdf)[3] <- 'NPP' #Name value column

taf.png(file.path(".","report","NPP_PolygonMap.png"),width =2100, height =1100)
gp <- ggplot(data = NPPdf) +
  geom_raster(mapping=aes(x=x, y=y, fill=NPP)) +
  scale_fill_gradientn(colours= rev(terrain.colors(10)), name='NPP',na.value = "grey50") +
  ggtitle(paste0("NPP (VGPM) - ",NPP.series[filenumber,]$date)) +
  xlab("longitude") +
  ylab("latitude") +
  theme_bw()
for(i in 1:length(polygon_name)){
  gp <- gp +
    geom_polygon(data=Poly[[i]],mapping=aes(x=long,y=lat),fill="grey50",alpha=0.2,colour="black") 
}
print(gp)
dev.off()

# compute NPP for the selected polygons for 8d periods ---------------------------------
NPP.resampled<-list()

for (i in 1:dim(NPP.series)[1]){
  NPP <- raster::stack(file.path(".","data",paste0(NPP.series$filename[i],"crop.gri")))
  NAvalue(NPP) <- -999 # transform -999 in NAs
  NPP.in.poly<- map_dfr(1:length(polygon_name),function(np){
    raster::extract(NPP,Poly[[np]],extract=TRUE)[[1]] %>% # extract a vector with all observations in the polygon for one date
      tibble() # and format it as a tibble
  }) %>% deframe()
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
NPP.series <- NPP.series%>% # only retain dates with valid estimates
  filter(!is.na(NPP.mean))


# compute NPP for the selected polygon - annual estimates ---------------------------------
NPP.annual <- tibble(year = unique(NPP.series$year),NPP.mean=NA,NPP.025=NA,NPP.975=NA)
if (resampling==TRUE){
  NPP.annual[,2:4] <- purrr::map_df(NPP.annual$year, function(year){
    w <- which(NPP.series$year==year) # identify valid individual files within a year
    NPPmat <- do.call(cbind,NPP.resampled[w]) # construct a matrix of resampled means with nboot lines
    out <-quantile(apply(NPPmat,1,sum)*8/1000,c(0.5,0.025,0.975))
  })
} else {
  NPP.annual <- NPP.series %>%
    group_by(year) %>%
    summarise(year=first(year),NPP.mean=mean(NPP.mean,na.rm=TRUE),NPP.025=NA,NPP.975=NA)
}


# hierarchical model ------------------------------------------------------
# transform data into groupedData format
datastruct <- NPP.series %>%
  groupedData(NPP.mean~doy|year,data = .)
# fit nlme model
nlmefit <- nlme(NPP.mean~(doy<x0)*(dsigm(doy,k1,x0,xmax))+(doy>=x0)*(dsigm(doy,k2,x0,xmax)),
                fixed = k1 + k2 + x0 + xmax ~1,
                random = k1 + k2 + x0 + xmax ~1,
                start=c(k1=.03,k2=.03,x0=180,xmax=45000),
                data = datastruct)
# make predictions
NPP.series$NPP.pred <- predict(nlmefit,newdata = datastruct) # model fit (predictions)
NPP.series$NPP.pred0 <- predict(nlmefit,newdata = datastruct,level=0) # model fit for fixed effects only (predictions at level zero)

# calculate integrated production over the period: day 35 to day 273
newdata1 <- tibble(year=rep(unique(NPP.series$year),each=239),doy=rep(35:273,length(unique(NPP.series$year)))) 
newdata1$NPP <- predict(nlmefit,newdata = newdata1)
newdata1 <- newdata1 %>% 
  group_by(year) %>% 
  mutate(cNPP = cumsum(NPP))
cNPP <- newdata1 %>% 
  group_by(year) %>% 
  summarise(totNPP=max(cNPP)/1000) %>% 
  arrange(year)

# extract timing of peak production
timing0 <- nlmefit$coefficients$fixed[3]+nlmefit$coefficients$random$year[,3]
timing <- tibble(year=as.integer(names(timing0)),peak=timing0) %>% arrange(year)
# calculate timing of start and end of NPP season
oneYNPP <- tibble(doy = 35:273,
                  NPP = predict(nlmefit,newdata = tibble(doy=35:273,year=NA),level = 0)) # predictions with fixed effect only (common to all years)
start_threshold <- max(oneYNPP$NPP)*0.1 # using as a starting date when PP reach up 10% of the average maximum
end_threshold <- max(oneYNPP$NPP)*0.5 # using as an ending date when PP reach down 50% of the average maximum
mean_date_of_peak <- nlmefit$coefficients$fixed[3]

timingstart <- newdata1 %>% 
  filter((doy<mean_date_of_peak)&(NPP>start_threshold)) %>% 
  group_by(year) %>%
  summarise(start=min(doy))
timingend <- newdata1 %>% 
  filter((doy>mean_date_of_peak)&(NPP>end_threshold)) %>% 
  group_by(year) %>%
  summarise(end=max(doy))
NPP.annual <- NPP.annual %>% # Construct the final NPP.annual table with total production and timing.
  left_join(cNPP,by="year") %>%
  left_join(timing,by="year") %>%
  left_join(timingstart,by="year") %>%
  left_join(timingend,by="year") %>%
  mutate(duration=end-start)
# Note that the totNPP estimates are in gC.m-2.y-1, while the mean estimates are in mgC.m-2.d-1

write.taf(NPP.series,file.path(".","model","NPPseries.txt"))
write.taf(NPP.annual,file.path(".","model","NPPannual.txt"))

