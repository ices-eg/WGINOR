## Prepare plots and tables for report

# libraries --------------------------------------------
library(icesTAF)
require(tidyverse)
mkdir("output")

# load model results ------------------------------------------------------
NPP.series <- read.taf(file.path(".","model","NPPseries.txt"))
NPP.annual <- read.taf(file.path(".","model","NPPannual.txt"))

# Figures -----------------------------------------------------------------
# 1 - multiple panel plot (individual years) with 8-day observations and hierarchical model 
ggplot() +
  geom_path(aes(x=doy,y=NPP.pred0),data = NPP.series, size = 0.5, col = "deepskyblue") +
  geom_path(aes(x=doy,y=NPP.pred),data = NPP.series, size = 1, col = "firebrick") +
  geom_point(aes(x=doy,y=NPP.mean),data = NPP.series,size = 0.5, col = "black") +
  geom_ribbon(aes(x=doy,ymax=NPP.pred,ymin=0),data = NPP.series, fill = "firebrick",alpha=0.1) +
  xlim(c(1,365)) +
  #geom_abline(intercept = c(start_threshold,end_threshold),slope=0,lty=2) +
  facet_wrap(~year,ncol=5) +
  ylab(expression(paste("NPP (mg.C.",m^-2,".",day^-1,")",sep="")))

# 2 - matrix type plot with start, peak and end of production season
ggplot(data=NPP.series) +
  geom_tile(aes(x=doy,y=year,fill=NPP.mean)) +
  geom_path(aes(x=peak,y=year),data=NPP.annual,size=1,col="black") +
  geom_path(aes(x=start,y=year),data=NPP.annual,col="black") +
  geom_path(aes(x=end,y=year),data=NPP.annual,col="black") +
  scale_fill_gradient(low="lightyellow",high = "seagreen") +
  labs(fill=expression(paste("NPP (mg.C.",m^-2,".",day^-1,")",sep="")))

# 3 - barplot with the NPP integrated over the observation season
ggplot(data=NPP.annual,aes(x=year,y=totNPP)) +
  geom_bar(stat = "identity") +
  ylim(c(0,NA)) +
  ylab(expression(paste("g.C.",m^-2,".",y^-1,sep="")))

