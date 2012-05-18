antsr_frequency_filter <- function( values , tr , freqLo , freqHi , nuis = NA )
{
  if( ( class( values ) != "matrix" && class( values ) != "data.frame" ) || length( dim( values ) ) != 2 )
  {
    print( "'values' must be a 2D 'numeric array'" )
    return( NULL )
  }
  if( class( tr ) != "numeric" || length( tr ) != 1 )
  {
    print( "'tr' must be a 'numeric value'" )
    return( NULL )
  }
  if( class( freqLo ) != "numeric" || length( freqLo ) != 1 )
  {
    print( "'freqLo' must be a 'numeric value'" )
    return( NULL )
  }
  if( class( freqHi ) != "numeric" || length( freqHi ) != 1 )
  {
    print( "'freqHi' must be a 'numeric value'" )
    return( NULL )
  }
  
  library("signal")
  library("timeSeries")
  library("mFilter")
  library("MASS")

  useNuis <- FALSE
  if( !is.na(nuis) )
  {
    useNuis <- TRUE
  }
  nvox1<-dim(values)[2]
  ntimeseries<-dim(values)[1]

  # first calculate the filter width for the butterworth based on TR and the desired frequency
  voxLo=round((1/freqLo)) # remove anything below this (high-pass)
  voxHi=round((1/freqHi))   # keep anything above this

  myTimeSeries<-ts(values,frequency=1.0/tr)
  if( useNuis )
    {
      print("robust residualizing regression")
      # myTimeSeries<-residuals(lqs(myTimeSeries~1+as.matrix(nuis)))
      myTimeSeries<-residuals(lm(myTimeSeries~1+as.matrix(nuis)))
    }
  myTimeSeries<-ts(myTimeSeries,frequency=1/tr)
  filteredTimeSeries<-residuals(cffilter(myTimeSeries,pl=voxHi,pu=voxLo,drift=T,type="t"))
  # fn<-(gsub('.csv','VisualizeTimeSeriesFiltering.pdf',valuesOut))
  # print(paste("writing out reference filtering result",fn))
  # pdf(fn)
  vv<-10
  par(mfrow=c(2,2))
  plot(myTimeSeries[,vv],type='l')
  spec.pgram( myTimeSeries[,vv], taper=0, fast=FALSE, detrend=F,demean=F, log="n")
  plot(filteredTimeSeries[,vv],type='l')
  spec.pgram( filteredTimeSeries[,vv], taper=0, fast=FALSE, detrend=F,demean=F, log="n")
  dev.off()
  #write.csv(filteredTimeSeries,valuesOut,row.names = F,q=T)
  return( filteredTimeSeries )
}
