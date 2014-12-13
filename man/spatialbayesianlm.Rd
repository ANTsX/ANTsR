\name{spatialbayesianlm}
\alias{spatialbayesianlm}

\title{
spatially constrained bayesian regression function.
}

\description{
Take a standard lm result and use bayesian regression to impose spatial regularity.
}

\usage{
bsol<-spatialbayesianlm( mylm, ymat, mask, smth = 1,
  priorWeight = 1, nhood = NA,
  regweights = NA )
}

\arguments{
\item{mylm}{
  standard lm result of the form mylm<-lm(ymat~.)
}
\item{ymat}{
  outcome matrix - usually from imaging data
}
\item{mask}{
  mask with non-zero entries n-columns of ymat
}
\item{smth}{
  smoothness parameter
}
\item{priorWeight}{
weight on the prior
}
\item{nhood}{
size of neighborhood
}
\item{regweights}{
weights on rows - size of ymat
}
\item{smoothcoeffmat}{
prior coefficient matrix
}
}

\value{
bayesian regression solution is output as a list of images
}

\author{
Avants BB
}

\examples{
  # make some simple data
  \dontrun{
  fn<-"PEDS012_20131101_pcasl_1.nii.gz"
# image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
  asl<-antsImageRead(fn,4)
  tr<-antsGetSpacing(asl)[4]
  aslmean<-getAverageOfTimeSeries( asl )
  aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
  pcaslpre <- aslPerfusion( asl, interpolation="linear",
  dorobust=0, useDenoiser=NA, skip=1, useBayesian=0,
  moreaccurate=0, verbose=T, mask=aslmask ) # throw away lots of data
  # user might compare to useDenoiser=FALSE
  pcasl.parameters <- list( sequence="pcasl", m0=pcaslpre$m0 )
  aslmat<-timeseries2matrix(asl,aslmask)
  tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
  dv<-computeDVARS(aslmat)
  perfmodel<-lm( aslmat ~ tc + stats::poly(dv,4) ) # standard model
  ssp<-spatialbayesianlm( perfmodel, aslmat, aslmask,
    priorWeight=1.e2 ,smth=1.6, nhood=rep(2,3) )
  plotANTsImage( ssp[[1]], slices='2x16x2', axis=3 )
  }
}
