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
bigLMStats <- function(mylm, lambda = 0, includeIntercept = FALSE) {
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




#' Efficiently compute an image-based linear regression model (ilr)
#'
#' This function simplifies calculating p-values from linear models in which
#' there is a similar formula that is applied many times with a change in
#' image-based predictors.  Image-based variables are stored in the input
#' matrix list. They should be named consistently in the input formula and
#' in the image list.  If they are not, an error will be thrown.  All input
#' matrices should have the same number of rows and columns.
#'
#' @param dataFrame This data frame contains all relevant predictors except for
#' the matrices associated with the image variables.
#' @param voxmats The named list of matrices that contains the changing predictors.
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
#' mat2 = replicate( nsub, rnorm( nsub ) )
#' myform = " outcome ~ covar + vox "
#' df = data.frame( outcome = outcome, covar = covar )
#' result = ilr( df, list( vox = mat ), myform)
#' print( names( result ) )
#' print( rownames( result$pValue ) )
#' myform = " vox2 ~ covar + vox "
#' df = data.frame( outcome = outcome, covar = covar )
#' result = ilr( df, list( vox = mat, vox2=mat2 ), myform)
#' print( names( result ) )
#' print( rownames( result$pValue ) )
#'
#' @export ilr
#' @importFrom RcppEigen fastLm
ilr <- function( dataFrame,  voxmats, myFormula ) {
  vdf = data.frame( dataFrame )
  matnames = names( voxmats )
  if ( length( matnames ) == 0 ) stop( 'please name the input list entries')
  p = ncol( voxmats[[1]] )
  for ( k in 1:length( voxmats ) ) {
    vdf = cbind( vdf, voxmats[[k]][,1] )
    names( vdf )[  ncol( vdf ) ] = matnames[k]
    if ( ncol( voxmats[[k]] ) != p  )
      stop( paste( "matrix ", matnames[k], " does not have ", p, "entries" ) )
  }
  # get names from the standard lm
  temp = summary( lm( myFormula  , data=vdf))
  myModelMatrix = model.matrix( lm( myFormula  , data=vdf) )
  myrownames = rownames(temp$coefficients)
  mypvs = matrix( rep( NA, p * length( myrownames ) ),
    nrow = length( myrownames ) )
  myestvs = mypvs
  myervs = mypvs
  mytvs = mypvs
  mylm = lm( myFormula , data = vdf )
#  colnames( myestvs ) = colnames( myervs ) = colnames( mypvs ) = colnames( mytvs ) = paste('v',1:p,sep='')
  rownames( myestvs ) = rownames( myervs ) = rownames( mypvs ) = rownames( mytvs ) = myrownames
  mypredictions = matrix( nrow = nrow(voxmats[[1]]), ncol =  ncol(voxmats[[1]]) )
  mymodels = list()
  if ( ! usePkg( "RcppEigen" ) ) {
    print("Need RcppEigen package")
    } else {
    lvx = length( voxmats )
    loform = as.formula( myFormula )
    for ( n in 1:ncol( voxmats[[1]] ) ) {
      for ( k in 1:lvx ) {
        vdf[ ,  matnames[k] ] = voxmats[[k]][,n]
      }
      flmmod <- RcppEigen::fastLm( loform, data=vdf )
      mysumm = summary( flmmod )
      mycoef = coefficients( mysumm )
      myestvs[ , n ] = mycoef[,1]
      myervs[ , n ] = mycoef[,2]
      mytvs[ , n ] = mycoef[,3]
      mypvs[ , n ] = mycoef[,4]
      mypredictions[ , n ] = predict( flmmod )
#      mymodels[[ n ]] = flmmod
    }
  }
  return(
    list(
      estimate=myestvs,
      stdError=myervs,
      tValue=mytvs,
      pValue=mypvs,
      predictions = mypredictions,
      modelMatrix =  myModelMatrix ) )
#      models = mymodels ) )
}





#' Predict from ilr output
#'
#' This function computes a prediction given \code{ilr} output.
#'
#' @param ilrResult This output form ilr
#' @param dataFrame This data frame contains all relevant predictors except for
#' the matrices associated with the image variables.
#' @param voxmats The named list of matrices that contains the changing predictors.
#' @param myFormula This is a character string that defines a valid regression formula.
#' @return the predicted matrix.
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 100
#' trte = sample( 1:nsub )[1:70]
#' covarIn = rnorm( nsub )
#' mat = replicate( nsub, rnorm( nsub ) )
#' mat2 = replicate( nsub, rnorm( nsub ) )
#' outcomeAtVox = mat[ , 2 ]
#' # also try myform = " vox ~ covar + vox2 "
#' myform="outcome ~ covar + vox + vox2"
#' df = data.frame( outcome = outcomeAtVox, covar = covarIn )
#' result = ilr( df[trte,], list( vox = mat[trte,], vox2 = mat2[trte,] ), myform)
#' df2 = data.frame( covar = covarIn[-trte] )
#' pred = ilr.predict(  result, df2, list( vox = mat[-trte,], vox2=mat2[-trte,] ), myform )
#' max( cor( pred, outcomeAtVox[-trte] ) )
#'
#' @seealso \code{\link{ilr}}
#' @export ilr.predict
ilr.predict <- function(
  ilrResult,
  dataFrame,
  voxmats,
  myFormula )
{
  vdf = data.frame( dataFrame )
  loform = as.formula( myFormula )
  matnames = names( voxmats )
  if ( length( matnames ) == 0 ) stop( 'please name the input list entries')
  outcomevarname = trimws( unlist( strsplit( myFormula, "~" ) )[1] )
  outcomevarum = which( outcomevarname == matnames  )
  outcomeisconstant = FALSE
  if ( length( outcomevarum ) == 0 ) {
    outcomeisconstant = TRUE
    outcomevarum = which( colnames(vdf) == outcomevarname  )
  }
  if ( ! ( outcomevarname %in% names( vdf ) ) ) {
    vdf = data.frame( vdf, rnorm(nrow(voxmats[[1]])))
    names( vdf )[ ncol( vdf ) ] = outcomevarname
  }
  p = ncol( voxmats[[1]] )
  for ( k in 1:length( voxmats ) ) {
    vdf = cbind( vdf, voxmats[[k]][,1] )
    names( vdf )[  ncol( vdf ) ] = matnames[k]
    if ( ncol( voxmats[[k]] ) != p  )
      stop( paste( "matrix ", matnames[k], " does not have ", p, "entries" ) )
  }
  # get names from the standard lm
  refmdl = RcppEigen::fastLm( loform  , data=vdf)
  mypredictions = matrix( nrow = nrow(voxmats[[1]]), ncol =  ncol(voxmats[[1]]) )
  if ( ! usePkg( "RcppEigen" ) ) {
    stop("Need RcppEigen package")
  } else {
    lvx = length( voxmats )
    loform = as.formula( myFormula )
    for ( n in 1:ncol( voxmats[[1]] ) ) {
      for ( k in 1:lvx ) {
        vdf[ ,  matnames[k] ] = voxmats[[k]][,n]
      }
      mycoef = ilrResult$estimate[,n]
      x = model.matrix( refmdl$formula, vdf )
      mypredictions[ , n ] =  as.vector( x %*% mycoef )
    }
  }
  return( mypredictions )
}
