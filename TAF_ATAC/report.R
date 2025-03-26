## Prepare plots and tables for report

require(ggplot2)
require(tidyverse)
library(icesTAF)

sourceTAF("utilities")

mkdir("report")

#load results
load("./output/tables.Rdata")

# Step 1: produce all individual ggplots
plotlist<-list()
for (ycol in c(2:length(table.all))){ # loop on all time-series
  if (is_tibble(table.all[[ycol]])){
    plotlist[[ycol-1]]<-ggATAC(result=table.all[[ycol]])+
      xlim(c(1980,NA)) +
      ggtitle(paste0(info$FullName[ycol],
                     "\nData transformation: ",info$Transformation[ycol],
                     "\nAutoregressive process: AR(",info$AR[ycol],")")) +
      ylab(paste0("units: ",info$Unit[ycol])) +
      theme(plot.title = element_text(size = 8))
    if (info$Transformation[ycol]==TRUE) {
      plotlist[[ycol-1]] <- plotlist[[ycol-1]] +
        ylim(c(0,NA))
    }
  } else {
    plotlist[[ycol-1]]<-ggplot() +
      ggtitle(paste0(info$FullName[ycol],
                     "\nData transformation: ",info$Transformation[ycol],
                     "\nAutoregressive process: AR(",info$AR[ycol],")")) +
      theme(plot.title = element_text(size = 8))
    
  }
}

# Step 2: arrange ggplot by data categories and output them in png files
Categories <- unique(na.omit(info$Category)) # identify data categories
Categories <- Categories[Categories!="NaN"] # remove NaN as a category
for(j in 1:length(Categories)){ # loop on categories
  l<-which(info$Category==Categories[j]) # id's of plots in the category
  NbFig<-ceiling(length(l)/12) # Nb of png to be created for a category
  for(k in 1:NbFig){ # loop on pages within categories
    taf.png(paste0("ATAC_series_",Categories[j],"_plot",k),width =2100, height =2970)
    title1 <- ggpubr::text_grob(paste0(Categories[j]," (",k,")"), size = 16, face = "bold")
    gridExtra::grid.arrange(grobs=plotlist[na.omit(l[(12*(k-1))+c(1:12)])-1],
                            ncol=2,
                            nrow=6,
                            top = title1)
    dev.off()
  }
}



