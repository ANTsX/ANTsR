\name{bayesianlm}
\alias{bayesianlm}

\title{
Simple bayesian regression function.
}

\description{
Take a prior mean and precision matrix for the regression solution
and uses them to solve for the regression parameters.  See Wikipedia.
}

\usage{
bsol<-bayesianlm( X, y, priorMean, precisionMatrix  )
}

\arguments{
\item{X}{
  data matrix
}
\item{y}{
  outcome
}
\item{priorMean}{
  expected parameters
}
\item{precisionMatrix}{
  inverse covariance matrix of the parameters - strength of the prior
}
}

\value{
bayesian regression solution is output
}

\author{
Avants BB
}

\examples{
  asl<-antsImageRead("PEDS012_20131101_pcasl_1.nii.gz",4)
  # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
  tr<-antsGetSpacing(asl)[4]
  aslmean<-getAverageOfTimeSeries( asl )
  aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
  aslmat<-timeseries2matrix(asl,aslmask)
  tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
  dv<-computeDVARS(aslmat)
  # do some bayesian learning based on the data
  perfmodel<-lm( aslmat ~ tc + dv ) # standard model
  X<-model.matrix( perfmodel )
  perfmodel<-lm( aslmat ~ tc + dv )
  prior  <- rowMeans( perfmodel$coefficients  )
  invcov <- solve( cov( t( perfmodel$coefficients ) ) )
  bayesianperfusion<-rep(0,ncol(aslmat))
  for ( i in 1:ncol(aslmat) )
    {
    blm<-bayesianlm(  X, aslmat[,i], prior, invcov )
    bayesianperfusion[i]<-blm$beta.t[1]
    }
  perfimg<-antsImageClone(aslmask)
  perfimg[ aslmask == 1 ]<-bayesianperfusion
  antsImageWrite(perfimg,'perf.nii.gz')
}
