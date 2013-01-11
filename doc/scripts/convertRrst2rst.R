#!/usr/bin/env Rscript 
# get the script name (only works when invoked with Rscript).
library(ANTsR)
Args <- commandArgs()
self<-Args[4]
self<-substring(self,8,nchar(as.character(self)))
fn<-(Args[6])
ofn<-(Args[7])
require(knitr)
opts_chunk$set(echo=TRUE, fig.path='figs/antsr-', cache=TRUE)
knit('006-minimal.Rrst','006-minimal.rst')
