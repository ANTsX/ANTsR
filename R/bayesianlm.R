bayesianlm <- function( X, y, priorMean, priorPrecision,
  priorIntercept = 0, regweights ) {
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
  if (veccoef)
    beta <- mu_n[-1] else beta <- mu_n[-1, ]
  preds <- X %*% mu_n
  b_n  <- priorIntercept + mean(regweights %*% y)-mean(regweights %*% preds)
  preds <- preds + b_n
  myresiduals<-( y - preds )
  if (dim(X)[2] > 2) {
    mycoefs <- diag(XtXinv[2:dim(X)[2], 2:dim(X)[2]])
  } else {
    mycoefs <- XtXinv[2, 2]
  }
  if ( veccoef ) {
    beta.std <- sqrt(sum((myresiduals)^2)/dfe * mycoefs)
  } else {
    beta.std <- t(sqrt(as.vector(colSums((myresiduals)^2)/dfe) %o%
      mycoefs))
  }
  if (veccoef)
    beta.t <- mu_n[-1]/beta.std
  if (!veccoef)
    beta.t <- mu_n[-1, ]/beta.std
  beta.pval <- 2 * pt(-abs(beta.t), df = dfe )

  pckg <- try(require(mvtnorm))
  betapost<-phi<-0
  if (pckg) {
    # lots of problems below - needs work ... but basically, this crudely
    # estimates integrals of the posterior across parameters
    # compute posterior for gamma
    errterm = (abs( myresiduals )) # my error distribution
    #  library(MASS)
    # gamma_parms <- fitdistr(errterm,"gamma")
    # or use residuals directly as theory says
    b_1 = priorIntercept + 0.5 * mean( errterm )
    # compute posterior for beta
    # FIXME - quantiles tied to error here
    #  pgam<-pgamma(quantile(errterm), shape=fitdistr(errterm,'gamma')$estimate[1] )
    #  phi1<-max( pgam )
    quanterr<-quantile( errterm )
    phi<-mean( pgamma( quanterr, shape = b_1 ) ) # mean across error quantiles
    sig1<-solve( tXX + phi* priorPrecision )
    mu1<-as.numeric( mu_n )
    smallpurt<-abs(mu1)*0.5
    # below gives mean posterior for beta across a range
    betapost <- pmvnorm( mu1-smallpurt, mu1+smallpurt, mean=mu1, sigma=sig1)[1]
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
