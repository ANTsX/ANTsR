\name{clusterTimeSeries}
\alias{clusterTimeSeries}

\title{
Split time series image into k distinct images
}

\description{
Uses clustering methods to split a time series into
similar subsets.
}

\usage{
cimgs<-clusterTimeSeries(  img, mask, krange=2:10, nsvddims=NA  )
}

\arguments{
\item{img}{
  input image
}
\item{mask}{
  mask to use
}
\item{krange}{
  k cluster range to explore
}
}

\value{
matrix is output
}

\author{
Avants BB
}

\examples{
\dontrun{
  if (!exists("fn") ) fn<-"PEDS029_20101110_pcasl_1.nii.gz"
   # high motion subject
  asl<-antsImageRead(fn,4)
  tr<-antsGetSpacing(asl)[4]
  aslmean<-getAverageOfTimeSeries( asl )
  aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
  omat<-timeseries2matrix(asl, aslmask )
  clustasl<-clusterTimeSeries( omat, krange=4:10 )
  for ( ct in 1:max(clustasl$clusters) )
    {
    sel<-clustasl$clusters != ct
    img<-matrix2timeseries( asl, aslmask, omat[sel,] )
    perf <- aslPerfusion( img, interpolation="linear",
      dorobust=0.9, useDenoiser=4, skip=10, useBayesian=0,
      moreaccurate=0, verbose=F, mask=aslmask )
    perfp <- list( sequence="pcasl", m0=perf$m0 )
    cbf <- quantifyCBF( perf$perfusion, perf$mask, perfp )
    ofn<-paste('temp',ct,'.nii.gz',sep='')
    antsImageWrite( cbf$kmeancbf , ofn )
    ct<-ct+1
    }
  }
}
