#!/usr/bin/env Rscript 
# get the script name (only works when invoked with Rscript).
library(ANTsR)
require(knitr)
Args <- commandArgs()
self<-Args[4]
self<-substring(self,8,nchar(as.character(self)))
fn<-(Args[6])
ofn<-(Args[7])
myrrst<-c("phantom_population_study.Rrst","plotANTsImage.Rrst","restingstatefMRIconnectivity.Rrst")
for ( myfn in myrrst ) {
  rstfn<-sub('.Rrst','.rst',myfn)
  knit(myfn,rstfn)
  rstfn<-paste("../../demo/",sub('.Rrst','.R',myfn),sep='')
  knit(myfn,rstfn,tangle=TRUE)
}
