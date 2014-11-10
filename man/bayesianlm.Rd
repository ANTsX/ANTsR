\name{bayesianlm}
\alias{bayesianlm}

\title{
Simple bayesian regression function.
}

\description{
Take a prior mean and precision matrix for the regression solution
and uses them to solve for the regression parameters.  The Bayesian
model, here, is on the multivariate distribution of the parameters.
}

\usage{
bsol<-bayesianlm( X, y, priorMean, precisionMatrix, weights )
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
  y<-rowMeans(aslmat)
  perfmodel<-lm( y ~ tc + dv ) # standard model
  tlm<-bigLMStats( perfmodel )
  X<-model.matrix( perfmodel )
  blm<-bayesianlm(  X, y )
  print( tlm$beta.p )
  print( blm$beta.p )
  # do some bayesian learning based on the data
  perfmodel<-lm( aslmat ~ tc + dv ) # standard model
  X<-model.matrix( perfmodel )
  perfmodel<-lm( aslmat ~ tc + dv )
  bayesianperfusionloc<-rep(0,ncol(aslmat))
  smoothcoeffmat<-perfmodel$coefficients
  nmatimgs<-list()
  for ( i in 1:nrow(smoothcoeffmat) )
    {
    temp<-antsImageClone( aslmask )
    temp[ aslmask == 1 ] <- smoothcoeffmat[i,]
#    ImageMath(3,temp,"PeronaMalik",temp,150,10)
#    SmoothImage(3,temp,1.5,temp)
    nmatimgs[[i]]<-antsGetNeighborhoodMatrix(temp,aslmask,
      rep(1,3), boundary.condition = "mean")
    smoothcoeffmat[i,]<-temp[ aslmask==1 ]
    }
  prior  <- rowMeans( smoothcoeffmat  )
  invcov <- solve( cov( t( smoothcoeffmat ) ) )
  blm2<-bayesianlm(  X, y, prior, invcov*100 )
  print( blm2$beta.p )
  for ( i in 1:ncol(aslmat) )
    {
    parammat<-nmatimgs[[1]][,i]
    for ( k in 2:length(nmatimgs))
      parammat<-cbind( parammat, nmatimgs[[k]][,i] )
    locinvcov<-solve( cov( parammat ) )
    localprior<-(smoothcoeffmat[,i])
    blm<-bayesianlm(  X, aslmat[,i], localprior, locinvcov*1000 )
    bayesianperfusionloc[i]<-blm$beta[1]
    }
  perfimg<-antsImageClone(aslmask)
  basicperf<-bigLMStats( perfmodel )$beta[1,]
  perfimg[ aslmask == 1 ]<-basicperf
  antsImageWrite(perfimg,'perf.nii.gz')
  perfimg[ aslmask == 1 ]<-bayesianperfusionloc
  antsImageWrite(perfimg,'perf_bayes.nii.gz')
  print( cor.test(basicperf, perfimg[ aslmask == 1 ] ) )
}
