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
  library(fpc)
  if (!exists("fn") ) fn<-"PEDS029_20101110_pcasl_1.nii.gz"
   # high motion subject
  asl<-antsImageRead(fn,4)
  tr<-antsGetSpacing(asl)[4]
  aslmean<-getAverageOfTimeSeries( asl )
  aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
  clustasl<-clusterTimeSeries( asl, aslmask, 2:10 )
  for ( img in clustasl$kimgs )
    {
    perf <- aslPerfusion( img, interpolation="linear",
      dorobust=0, useDenoiser=NA, skip=10, useBayesian=0,
      moreaccurate=1, verbose=F, mask=aslmask )
    perfp <- list( sequence="pcasl", m0=perf$m0 )
    cbf <- quantifyCBF( perf$perfusion, perf$mask, perfp )
meancbf <- cbf$kmeancbf
print(mean(meancbf[ pcasl.bayesian$mask==1 ]))
antsImageWrite( meancbf , 'temp.nii.gz')

    }
}
}
