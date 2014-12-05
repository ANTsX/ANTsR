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
weights on rows
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
  aslmat<-timeseries2matrix(asl,aslmask)
  tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
  dv<-computeDVARS(aslmat)
  # do some comparison with a single y, no priors
  perfmodel<-lm( aslmat ~ tc + dv ) # standard model
  ssp<-spatialbayesianlm( perfmodel, aslmat, aslmask, priorWeight=1.e5 ,smth=3)
  }
}
