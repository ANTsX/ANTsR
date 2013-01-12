#!/usr/bin/env Rscript 
# get the script name (only works when invoked with Rscript).
library(ANTsR)
require(knitr)
Args <- commandArgs()
self<-Args[4]
self<-substring(self,8,nchar(as.character(self)))
fn<-(Args[6])
ofn<-(Args[7])
myrrst<-c("phantom_population_study.Rrst")
for ( myfn in myrrst ) {
  rstfn<-sub('.Rrst','.rst',myfn)
  print(paste(myfn,'2',rstfn))
  knit(myfn,rstfn)
}
