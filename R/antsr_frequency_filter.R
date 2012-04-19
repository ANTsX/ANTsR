antsr_frequency_filter <- function( ... )
{
#!/usr/bin/env Rscript
#Args <- commandArgs()
Args <- as.character( c(...) ) ;
library("signal")
library("timeSeries")
library("mFilter")
library("MASS")
# print(paste("length of args is ", length(Args)))
if ( length(Args) < 5  ){
fnm<-Args[4]
fnm<-substring(fnm,8,nchar(as.character(fnm)))
print("Usage - antsr_frequency_filter( values_in.csv , values_out.csv , TR , freq_lo , freq_hi , nuis.csv )" )
print(paste(" ... band pass filter and residualize the time series if nuis is  passed in " ))
print(paste(" freq_lo --- throw away signal below this frequency " ))
print(paste(" freq_hi --- throw away signal above this frequency " ))
return ;
}
ARGIND<-1
valuesIn<-c(as.character(Args[ARGIND]))
ARGIND<-ARGIND+1
valuesOut<-c(as.character(Args[ARGIND]))
ARGIND<-ARGIND+1
tr<-c(as.numeric(Args[ARGIND]))
ARGIND<-ARGIND+1
freqLo<-c(as.numeric(Args[ARGIND]))
ARGIND<-ARGIND+1
freqHi<-c(as.numeric(Args[ARGIND]))
ARGIND<-ARGIND+1
nuis<-NA
useNuis<-FALSE
if ( length(Args) > 5 )
{
  nuiscsv<-c(as.character(Args[ARGIND]))
  nuis<-read.csv(nuiscsv)
  useNuis<-TRUE
}
print(paste('read data',valuesIn))
values<-read.csv(valuesIn)
nvox1<-dim(values)[2]
ntimeseries<-dim(values)[1]
# first calculate the filter width for the butterworth based on TR and the desired frequency
voxLo=round((1/freqLo)) # remove anything below this (high-pass)
voxHi=round((1/freqHi))   # keep anything above this
print(paste("start filtering smoothing by",voxHi," and ",voxLo))
myTimeSeries<-ts(values,frequency=1.0/tr)
if ( useNuis )
  {
  print("robust residualizing regression")
#  myTimeSeries<-residuals(lqs(myTimeSeries~1+as.matrix(nuis)))
  myTimeSeries<-residuals(lm(myTimeSeries~1+as.matrix(nuis)))
  }
myTimeSeries<-ts(myTimeSeries,frequency=1/tr)
filteredTimeSeries<-residuals(cffilter(myTimeSeries,pl=voxHi,pu=voxLo,drift=T,type="t"))
fn<-(gsub('.csv','VisualizeTimeSeriesFiltering.pdf',valuesOut))
print(paste("writing out reference filtering result",fn))
pdf(fn)
vv<-10
par(mfrow=c(2,2))
plot(myTimeSeries[,vv],type='l')
spec.pgram( myTimeSeries[,vv], taper=0, fast=FALSE, detrend=F,demean=F, log="n")
plot(filteredTimeSeries[,vv],type='l')
spec.pgram( filteredTimeSeries[,vv], taper=0, fast=FALSE, detrend=F,demean=F, log="n")
dev.off()
write.csv(filteredTimeSeries,valuesOut,row.names = F,q=T)
}