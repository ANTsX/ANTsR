#' Efficiently compute basic statistical inference from regressions with
#' multiple outcomes
#'
#' This function simplifies calculating p-values from linear models in which
#' there are many outcome variables, such as in voxel-wise regressions.  To
#' perform such an analysis in R, you can concatenate the outcome variables
#' column-wise into an n by p matrix \code{y}, where there are n subjects and p
#' outcomes (see \code{Examples}).  Calling \code{lm(y~x)} calculates the
#' coefficients, but statistical inference is not provided.  This function
#' provides basic statistical inference efficiently.
#'
#'
#' @param mylm Object of class \code{lm}.
#' @param lambda Value of ridge penalty for inverting ill-conditioned matrices.
#' @param includeIntercept Whether or not to include p-values for intercept
#' term in result.
#' @return A list containing objects: \item{fstat}{F-statistic of whole model
#' (one value per outcome).} \item{pval.model}{p-value of model (one value per
#' outcome).} \item{beta}{Values of coefficients (one value per predictor per
#' outcome).} \item{beta.std}{Standard error of coefficients.}
#' \item{beta.t}{T-statistic of coefficients.} \item{beta.pval}{p-value of
#' coefficients.}
#' @author Kandel BM.
#' @examples
#'
#'
#' nsub <- 100
#' set.seed(1500)
#' x <- 1:nsub
#' y <- matrix(c(x + rnorm(nsub), sin(x)), nrow=nsub)
#' x <- cbind(x, x^2)
#' y1 <- y[, 1]
#' y2 <- y[, 2]
#' lm1 <- lm(y1~x)
#' lm2 <- lm(y2~x)
#' mylm <- lm(y ~ x)
#'
#' myest <- bigLMStats(mylm)
#' print(paste("R beta estimates for first outcome is", summary(lm1)$coefficients[-1,1],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,1]))
#' print(paste("and our estimate is", as.numeric(myest$beta[,1]), as.numeric(myest$beta[,2])))
#' print(paste("R std error estimate for first outcome is", summary(lm1)$coefficients[-1,2],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,2],
#'             "and our estimate is", myest$beta.std[,1], myest$beta.std[,2]))
#' print(paste("R t value estimate for first outcome is", summary(lm1)$coefficients[-1,3],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,3],
#'             "and our estimate is", myest$beta.t[,1], myest$beta.t[,2]))
#' print(paste("R pval for first outcome is", summary(lm1)$coefficients[-1,4],
#'             "and for second outcome is", summary(lm2)$coefficients[-1,4],
#'             "and our estimate is", myest$beta.pval[,1], myest$beta.pval[,2]))
#'
#' @export bigLMStats
bigLMStats <- function(mylm, lambda = 0, includeIntercept = F) {
  veccoef <- FALSE
  if (is.null(dim(mylm$coefficients)))
    veccoef <- TRUE
  if (!includeIntercept) {
    if (veccoef)
      beta <- mylm$coefficients[-1] else beta <- mylm$coefficients[-1, ]
  } else beta <- mylm$coefficients
  myresponse <- model.response(model.frame(mylm))
  X <- model.matrix(mylm)
  dfr <- dim(X)[2] - 1
  dfe <- dim(X)[1] - dfr - 1
  if (is.vector(myresponse)) {
    msm <- sum((t(t(mylm$fitted.values) - mean(myresponse)))^2)/dfr
    mse <- sum((mylm$residuals)^2)/dfe
    fstat <- msm/mse
  } else {
    msm <- colSums((t(t(mylm$fitted.values) - colMeans(myresponse)))^2)/dfr
    mse <- colSums((mylm$residuals)^2)/dfe
    fstat <- msm/mse
  }
  pval.model <- pf(fstat, dfr, dfe, lower.tail = F)
  XtXinv <- solve(t(X) %*% X + diag(ncol(X)) * lambda)
  if (!includeIntercept) {
    if (dim(X)[2] > 2) {
      mycoefs <- diag(XtXinv[2:dim(X)[2], 2:dim(X)[2]])
    } else {
      mycoefs <- XtXinv[2, 2]
    }
  } else mycoefs <- diag(XtXinv[1:dim(X)[2], 1:dim(X)[2]])
  if (is.vector(mylm$residuals)) {
    beta.std <- sqrt(sum((mylm$residuals)^2)/mylm$df.residual * mycoefs)
  } else {
    beta.std <- t(sqrt(as.vector(colSums((mylm$residuals)^2)/mylm$df.residual) %o%
      mycoefs))
  }
  if (!includeIntercept) {
    if (veccoef)
      beta.t <- mylm$coefficients[-1]/beta.std
    if (!veccoef)
      beta.t <- mylm$coefficients[-1, ]/beta.std
  } else beta.t <- mylm$coefficients/beta.std
  beta.pval <- 2 * pt(-abs(beta.t), df = mylm$df.residual)
  list(fstat = fstat, pval.model = pval.model, beta = beta, beta.std = beta.std,
    beta.t = beta.t, beta.pval = beta.pval)
}




#' Efficiently compute a voxel-wise varying linear regression model
#'
#' This function simplifies calculating p-values from linear models in which
#' there is a similar formula that is applied many times with a change in only
#' one predictor.  The outcome variable is constant.  The changing variable
#' should be named \code{vox} in the input formula.
#'
#' @param dataFrame This data frame contains all relevant predictors except for
#' the matrix associated with the changing variable, heretofore named \code{vox}.
#' @param voxmat The matrix that contains the changing predictor named \code{vox}.
#' @param myFormula This is a character string that defines a valid regression formula.
#' @return A list of different matrices that contain names derived from the
#' formula and the coefficients of the regression model.
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 100
#' outcome = rnorm( nsub )
#' covar = rnorm( nsub )
#' mat = replicate( nsub, rnorm( nsub ) )
#' myform = " outcome ~ covar + vox "
#' df = data.frame( outcome = outcome, covar = covar )
#' result = bigLMStats2( df, mat, myform)
#' print( names( result ) )
#' print( rownames( result$pValue ) )
#'
#' @export bigLMStats2
bigLMStats2 <- function( dataFrame,  voxmat, myFormula ) {
  vdf = data.frame( dataFrame, vox = voxmat[,1] )
  temp = summary( lm( myFormula  , data=vdf))
  myrownames = rownames(temp$coefficients)
  mycolnames = colnames( voxmat )
  mypvs = matrix( rep( NA, ncol( voxmat ) * length( myrownames ) ),
    nrow = length( myrownames ) )
  myestvs = mypvs
  myervs = mypvs
  mytvs = mypvs
  colnames( myestvs ) = colnames( myervs ) = colnames( mypvs ) = colnames( mytvs ) = mycolnames
  rownames( myestvs ) = rownames( myervs ) = rownames( mypvs ) = rownames( mytvs ) = myrownames
  if ( ! usePkg( "RcppEigen" ) ) {
    print("Need RcppEigen package")
    } else {
    loform = as.formula( myFormula )
    for ( n in 1:ncol( voxmat ) ) {
      vdf$vox = voxmat[,n]
      flmmod <- fastLm( loform, data=vdf )
      mysumm = summary( flmmod )
      mycoef = coefficients( mysumm )
      myestvs[ , n ] = mycoef[,1]
      myervs[ , n ] = mycoef[,2]
      mytvs[ , n ] = mycoef[,3]
      mypvs[ , n ] = mycoef[,4]
    }
  }
  return(
    list(
      estimate=myestvs,
      stdError=myervs,
      tValue=mytvs,
      pValue=mypvs ) )
}
