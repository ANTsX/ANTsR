#' Simple bayesian regression function.
#' 
#' Take a prior mean and precision matrix for the regression solution and uses
#' them to solve for the regression parameters.  The Bayesian model, here, is
#' on the multivariate distribution of the parameters.
#' 
#' 
#' @param X data matrix
#' @param y outcome
#' @param priorMean expected parameters
#' @param precisionMatrix inverse covariance matrix of the parameters -
#' strength of the prior
#' @param includeIntercept include the intercept in the model
#' @return bayesian regression solution is output
#' @author Avants BB
#' @examples
#' 
#'   # make some simple data
#'   set.seed(1)
#'   n<-20
#'   rawvars<-sample(1:n)
#'   nois<-rnorm(n)
#'   # for some reason we dont know age is correlated w/noise
#'   age<-as.numeric(rawvars)+(abs(nois))*sign(nois)
#'   wt<-( sqrt(rawvars) + rnorm(n) )
#'   mdl<-lm( wt ~ age + nois )
#'   summary(mdl)
#'   X<-model.matrix( mdl )
#'   priorcoeffs<-coefficients(mdl)
#'   covMat<-diag(length(priorcoeffs))+0.1
#'   # make some new data
#'   rawvars2<-sample(1:n)
#'   nois2<-rnorm(n)
#'   # now age is correlated doubly w/noise
#'   age2<-as.numeric(rawvars2)+(abs(nois2))*sign(nois2)*2.0
#'   wt2<-( sqrt(rawvars2) + rnorm(n) )
#'   mdl2<-lm( wt2 ~ age2 + nois2 )
#'   X2<-model.matrix( mdl2 )
#'   precisionMat<-solve( covMat )
#'   precisionMat[2,2]<-precisionMat[2,2]*1.e3 # heavy prior on age
#'   precisionMat[3,3]<-precisionMat[3,3]*1.e2 # some prior on noise
#'   bmdl<-bayesianlm( X2, wt2, priorMean=priorcoeffs, precisionMat )
#'   bmdlNoPrior<-bayesianlm( X2, wt2 )
#'   print(priorcoeffs)
#'   print(bmdl$beta)
#'   print(bmdlNoPrior$beta)
#'   \dontrun{
#'   fn<-"PEDS012_20131101_pcasl_1.nii.gz"
#' # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
#'   asl<-antsImageRead(fn,4)
#'   tr<-antsGetSpacing(asl)[4]
#'   aslmean<-getAverageOfTimeSeries( asl )
#'   aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
#'   aslmat<-timeseries2matrix(asl,aslmask)
#'   tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
#'   dv<-computeDVARS(aslmat)
#'   # do some comparison with a single y, no priors
#'   y<-rowMeans(aslmat)
#'   perfmodel<-lm( y ~ tc + dv ) # standard model
#'   tlm<-bigLMStats( perfmodel )
#'   X<-model.matrix( perfmodel )
#'   blm<-bayesianlm(  X, y )
#'   print( tlm$beta.p )
#'   print( blm$beta.p )
#'   # do some bayesian learning based on the data
#'   perfmodel<-lm( aslmat ~ tc + dv ) # standard model
#'   X<-model.matrix( perfmodel )
#'   perfmodel<-lm( aslmat ~ tc + dv )
#'   bayesianperfusionloc<-rep(0,ncol(aslmat))
#'   smoothcoeffmat<-perfmodel$coefficients
#'   nmatimgs<-list()
#'   for ( i in 1:nrow(smoothcoeffmat) )
#'     {
#'     temp<-antsImageClone( aslmask )
#'     temp[ aslmask == 1 ] <- smoothcoeffmat[i,]
#' #    ImageMath(3,temp,"PeronaMalik",temp,150,10)
#'     SmoothImage(3,temp,1.5,temp)
#'     nmatimgs[[i]]<-antsGetNeighborhoodMatrix(temp,aslmask,
#'       rep(2,3), boundary.condition = "mean")
#'     smoothcoeffmat[i,]<-temp[ aslmask==1 ]
#'     }
#'   prior  <- rowMeans( smoothcoeffmat  )
#'   invcov <- solve( cov( t( smoothcoeffmat ) ) )
#'   blm2<-bayesianlm(  X, y, prior, invcov*1.e4 )
#'   print( blm2$beta.p )
#'   for ( i in 1:ncol(aslmat) )
#'     {
#'     parammat<-nmatimgs[[1]][,i]
#'     for ( k in 2:length(nmatimgs))
#'       parammat<-cbind( parammat, nmatimgs[[k]][,i] )
#'     locinvcov<-solve( cov( parammat ) )
#'     localprior<-(smoothcoeffmat[,i])
#'     blm<-bayesianlm(  X, aslmat[,i], localprior, locinvcov*1.e4 )
#'     bayesianperfusionloc[i]<-blm$beta[1]
#'     }
#'   perfimg<-antsImageClone(aslmask)
#'   basicperf<-bigLMStats( perfmodel )$beta[1,]
#'   perfimg[ aslmask == 1 ]<-basicperf
#'   antsImageWrite(perfimg,'perf.nii.gz')
#'   perfimg[ aslmask == 1 ]<-bayesianperfusionloc
#'   antsImageWrite(perfimg,'perf_bayes.nii.gz')
#'   print( cor.test(basicperf, perfimg[ aslmask == 1 ] ) )
#'   }
#' 
#' @export bayesianlm
bayesianlm <- function( X, y, priorMean, priorPrecision,
  priorIntercept = 0, regweights, includeIntercept = F ) {
  if ( is.null(dim(y)) ) veccoef<-TRUE else veccoef<-FALSE
# priorPrecision is the inverse of the covariance matrix
  if (  missing(priorPrecision) )
    priorPrecision<-diag( ncol(X) )*0
  if (  missing(priorMean) )
    priorMean<-rep( 0, ncol(X) )
  if ( !missing(regweights) )
    {
    regweights<-diag(regweights)
    }
  if (  missing(regweights) )
    {
    regweights<-rep(1,length(y))
    regweights<-diag(regweights)
    }
  dfr <- dim(X)[2] - 1
  dfe <- dim(X)[1] - dfr - 1
  tXX<-t(X) %*% ( regweights %*% X )
  XtXinv <- solve( tXX + priorPrecision )
  temp<-t(X) %*% ( regweights %*% y )
  X2 <- ( priorPrecision %*% priorMean + temp )
  mu_n <- XtXinv %*% X2
  if ( !includeIntercept )
    {
    if (veccoef )  beta <- mu_n[-1] else beta <- mu_n[-1, ]
    } else beta<-mu_n
  if ( includeIntercept )  priorIntercept=0
  preds <- X %*% mu_n
  b_n  <- priorIntercept + mean(regweights %*% y)-mean(regweights %*% preds)
  preds <- preds + b_n
  myresiduals<-( y - preds )

  if (!includeIntercept) {
    if (dim(X)[2] > 2) {
      mycoefs <- diag(XtXinv[2:dim(X)[2], 2:dim(X)[2]])
      } else {
        mycoefs <- XtXinv[2, 2]
      }
    } else mycoefs <- diag(XtXinv[1:dim(X)[2], 1:dim(X)[2]])

  if ( veccoef ) {
    beta.std <- sqrt(sum((myresiduals)^2)/dfe * mycoefs)
  } else {
    beta.std <- t(sqrt(as.vector(colSums((myresiduals)^2)/dfe) %o%
      mycoefs))
  }

  if (!includeIntercept){
    if (veccoef)
      beta.t <- mu_n[-1]/beta.std
    if (!veccoef)
      beta.t <- mu_n[-1, ]/beta.std
    } else beta.t <- mu_n / beta.std

  beta.pval <- 2 * pt(-abs(beta.t), df = dfe )

#  pckg <- try(require(mvtnorm))
  betapost<-phi<-0
#  if ( pckg & FALSE ) {
  if ( FALSE ) {
    # lots of problems below - needs work ... but basically, this crudely
    # estimates integrals of the posterior across parameters
    # compute posterior for gamma
    myresidualsmod<-myresiduals# remove scaling effects
    errterm = myresidualsmod^2 # my error distribution
    gamma_parms <- fitdistr(errterm,"gamma")
    # or use residuals directly as theory says
    b_1 = priorIntercept + 0.5 * mean( errterm, na.rm=T )
    sorterr<-sort(errterm)
    shest=fitdistr(sorterr,'gamma')$estimate[1]
    pgam<-pgamma( sorterr, shape=shest )
    phi<-mean( pgam, na.rm=T )
    sig1<-solve( tXX + phi* priorPrecision )
    mu1<-as.numeric( mu_n )
    # below gives mean posterior for beta across a range
    betapost <- pmvnorm( mu1,rep(Inf,length(mu1)), mean=mu1, sigma=sig1)[1]
  }
  return( list( beta = beta, beta.std = beta.std,
    beta.t = beta.t, beta.pval = beta.pval,
    fitted.values=preds,
    betaPosteriors=betapost,
    precisionPosteriors=phi,
    posteriorProbability=phi*betapost
     )
    )
}
