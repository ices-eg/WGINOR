## Prepare plots and tables for report

require(ggplot2)
require(tidyverse)
library(icesTAF)

sourceTAF("utilities")

mkdir("report")

load("./output/tables.Rdata")

# Step 1: produce all individual ggplots
plotlist<-list()
for (ycol in c(2:length(table.all))){ # loop on all time-series (except 65 which is too short)
  if (is_tibble(table.all[[ycol]])){
  plotlist[[ycol-1]]<-ggATAC(result=table.all[[ycol]])+
    xlim(c(1980,NA)) +
    ggtitle(paste0(info$FullName[ycol],
                   "\nData transformation: ",info$transformation[ycol],
                   "\nAutoregressive process: AR(",info$AR[ycol],")")) +
    ylab(paste0("units: ",info$unit[ycol]))
  } else {
    plotlist[[ycol-1]]<-ggplot() +
      ggtitle(paste0(info$FullName[ycol],
                     "\nData transformation: ",info$tranformation[ycol],
                     "\nAutoregressive process: AR(",info$AR[ycol],")"))
  }
}

# Step 2: arrange ggplot by data categories and output them in png files
Categories <- unique(na.omit(info$category)) # identify data categories
Categories <- Categories[Categories!="NaN"] # remove NaN as a category
for(j in 1:length(Categories)){ # loop on categories
  l<-which(info$category==Categories[j]) # id's of plots in the category
  NbFig<-ceiling(length(l)/12) # Nb of png to be created for a category
  for(k in 1:NbFig){ # loop on pages within categories
    taf.png(paste0("ATAC_series_",Categories[j],"_plot",k),width =2100, height =2970)
    title1 <- ggpubr::text_grob(paste0(Categories[j]," (",k,")"), size = 18, face = "bold")
    gridExtra::grid.arrange(grobs=plotlist[na.omit(l[(12*(k-1))+c(1:12)])-1],
                            ncol=3,
                            nrow=4,
                            top = title1)
    dev.off()
  }
}





