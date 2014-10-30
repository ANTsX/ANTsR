#!/usr/bin/env Rscript 
# get the script name (only works when invoked with Rscript).
library(ANTsR)
Args <- commandArgs()
self <- Args[4]
self <- substring(self, 8, nchar(as.character(self)))
fn <- (Args[6])
ofn <- (Args[7])
convertRd2rst(fn, ofn) 
