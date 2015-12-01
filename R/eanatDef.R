#' Eigenanatomy with deflation
#'
#' Simplified, low-parameter eigenanatomy implemented with deflation. The
#' algorithm is able to automatically select the key \code{nvecs} and hidden
#' \code{sparseness} parameters.  The user should select the \code{cthresh} and
#' \code{smoother} regularization parameters for his or her application and also
#' based on observing algorithm behavior when \code{verbose=TRUE}.
#'
#' @param inmat input matrix
#' @param nvecs number of eigenanatomy vectors to compute. if this is missing,
#' we use a data-driven method to select its value. the principle used here is
#' that we want few but sparse pseudo-eigenvectors that are minimally
#' correlated in row-space. true left and right eigenvectors are uncorrelated
#' in both row and column (left and right eigenvector) spaces, but this is not
#' the case when we impose sparsity.
#' @param mask input mask, must match matrix
#' @param smoother regularization parameter, typically 0 or 0.5
#' @param cthresh remove isolated voxel islands of size below this value
#' @param its number of iterations
#' @param eps gradient descent parameter
#' @param positivity return unsigned eigenanatomy vectors
#' @param maxNEvec integer that, if set greater than zero, indicates that we use
#' a low-rank approximation to the input matrix before proceeding to eanat.
#' this value should be greater than \code{nvecs}
#' @param selectorScale influences automatic selection of \code{nvecs} and tries
#' to find the knee in the correlation plot. This parameter produces fewer,
#' less sparse eigenanatomy pseudo-eigenvectors as its value increases.  Its
#' minimum value is 1 and a reasonable range is between 1 and 2.  The user
#' should look at the plot produced when verbosity is turned on.
#' @param verbose controls whether computation is silent or not.
#' @return matrix is output, analogous to \code{svd(mat,nu=0,nv=nvecs)}
#' @examples
#' mat <- matrix(rnorm(1000),ncol=50)
#' esol<-eanatDef( mat )
#' cor( mat %*% t(esol))
#' \dontrun{
#' mat <- matrix(rnorm(8000),ncol=50)
#' esol<-eanatDef( mat, selectorScale = 1.2 )
#' print(paste("selected", nrow(esol),'pseudo-eigenvectors'))
#' print( mean( abs( cor( mat %*% t(esol)) ) ) ) # what we use to select nvecs
#' }
#' @export eanatDef
eanatDef <- function( inmat, nvecs, mask=NA,
  smoother=0, cthresh=0, its=5, eps=0.1,
  positivity = FALSE, maxNEvec = 0, selectorScale=1.5,
  verbose=FALSE )
{
mat = scale( inmat )
if ( !positivity ) keeppos = (-1.0) else keeppos = (1.0)
if ( is.na(mask) ) {
  mask = makeImage( c(3,ncol(mat)+2), voxval=0 )
  mask[ 2, 2:(2+ncol(mat)-1) ] = 1
  }
if ( sum(mask==1) != ncol(mat) ) stop("Mask must match mat")
if ( selectorScale < 1 ) selectorScale = 1.1
if ( missing(nvecs) ) # selection procedure
  {
  if ( verbose ) print("estimate nvecs")
  mxn = nrow(mat)-1
  if ( maxNEvec > 1 & maxNEvec < mxn ) mxn = maxNEvec
  solutionmatrix = t( svd( mat, nu=0, nv=mxn )$v )
  mycorrs = rep( NA, mxn )
  if ( verbose ) progress <- txtProgressBar(min = 2, max = mxn, style = 3)
  foundNA = FALSE
  for ( xpn in 2:mxn )
    {
    if ( ! foundNA )
      {
      ilist = matrixToImages( solutionmatrix[1:xpn,], mask )
      eseg = eigSeg( mask, ilist,  TRUE, cthresh=cthresh  )
      temp = imageListToMatrix( ilist, mask )
      pp1 = mat %*% t( temp )
      mycorrs[xpn] = mean( abs( cor(pp1) ) )
      if ( is.na( mycorrs[xpn] ) ) foundNA = TRUE
      }
    if ( verbose ) setTxtProgressBar(progress, xpn)
    }
    if ( verbose ) close(progress)
    mycorrs[1] = max( mycorrs, na.rm=T )
    targetCorr = selectorScale * min( abs(mycorrs), na.rm=T  )
    nvecs = which( mycorrs <= targetCorr )
    nvecs = nvecs[1] # take 1st that meets criterion
    if ( verbose ) {
      plot( 1:mxn, ts(mycorrs), type='l', main='mean correlation versus nvecs' )
      points( nvecs, mycorrs[nvecs], col='red', cex = 2)
      print( paste( "selected:", nvecs ) )
      }
  }
if ( nvecs >= nrow(mat) ) nvecs = nrow( mat ) - 1
solutionmatrix = t( svd( mat, nu=0, nv=nvecs )$v )
pp1 = mat %*% t( solutionmatrix )
ilist = matrixToImages( solutionmatrix, mask )
eseg = eigSeg( mask, ilist,  TRUE, cthresh=cthresh  )
solutionmatrix = imageListToMatrix( ilist, mask )
sparvals = rep( NA, nvecs )
for ( i in 1:nvecs )
  sparvals[i] = sum( abs(solutionmatrix[i,]) > 0  ) / ncol( mat ) * keeppos
for ( sol in 1:nrow(solutionmatrix))
  {
  if ( sol == 1 ) rmat = mat else {
    pp = mat %*% t( solutionmatrix )
    rmat = scale( residuals( lm( mat ~ pp[ ,1:(sol-1)] ) ) )
  }
  vec = solutionmatrix[sol,]
  vec = vec / sqrt( sum( vec * vec ) ) # this is initial vector
  # now do projected gradient descent
  for ( i in 1:its )
    {
    grad = .bootSmooth( rmat, vec, smoother, mask )
    if ( i == 1 ) w1=1 else w1=1
    vec = vec*w1 + grad * eps
    vec = .eanatDefSparsifyV( vec, sparvals[sol], mask=mask,
      smoother=smoother, clustval=cthresh )
    if ( is.na(mean(vec))) vec = rnorm( length(vec) )
    vec = vec / sqrt( sum( vec * vec ) )
    rq = sum( vec * ( t(rmat) %*% ( rmat %*% vec ) ) )
    if ( verbose ) print( rq )
    }
  solutionmatrix[sol,]=vec
  pp = mat %*% t( solutionmatrix )
  errn = mean( abs(  mat -  predict( lm( mat ~ pp[,1:sol] ) ) ) )
  errni = mean( abs(  mat -  predict( lm( mat ~ pp1[,1:sol] ) ) ) )
  if ( verbose ) print(paste("sol",sol,"err",errn,"erri",errni))
  }
if ( verbose )
  print( paste( "MeanCor", mean(abs( cor( mat %*% t( solutionmatrix ) ) ) ) ))
sparvals2 = rep( NA, nvecs )
for ( i in 1:nvecs )
  sparvals2[i] = sum( abs(solutionmatrix[i,]) > 0  ) / ncol( mat )
if ( verbose ) print(sparvals)
if ( verbose ) print(sparvals2)
return( solutionmatrix )
}

.bootSmooth <- function( rmat, vec, smoother, mask )
  {
  if ( smoother == 0 )
    {
    grad = t( rmat ) %*% ( rmat %*% vec )
    grad = grad / sqrt( sum( grad * grad ) )
    return( grad )
    }
  else
    {
    grad = t( rmat ) %*% ( rmat %*% vec )
    grad = grad / sqrt( sum( grad * grad ) )
    gradi = makeImage( mask, grad )
    gradi = smoothImage( gradi, sigma=smoother, sigmaInPhysicalCoordinates=F )
    newvec = as.numeric( gradi[ mask > 0.5 ] )
    grad[] = newvec[]
    return( grad )
    }
  sgrad = vec * 0
  for ( i in 1:smoother )
    {
    myboot = sample( 1:nrow(rmat), replace=TRUE )
    bootmat = rmat[myboot,]
    grad = t( bootmat ) %*% ( bootmat %*% vec )
    grad = grad / sqrt( sum( grad * grad ) )
    sgrad = sgrad + grad
    }
  return( sgrad / sqrt( sum( sgrad * sgrad ) ) )
  }

.eanatDefSparsifyV <- function(vin, sparam, mask = NA,
  smoother=0, clustval = 0, verbose = F) {
  if (abs(sparam) >= 1)
    return(vin)
  if (nrow(vin) < ncol(vin))
    v <- t(vin) else v <- vin
  mysigns = sign( v )
  if ( sparam < 0 ) v <- v * mysigns
  b <- round(abs(as.numeric(sparam)) * nrow(v))
  if (b < 3)
    b <- 2
  if (b > nrow(v))
    b <- nrow(v)
  for (i in 1:ncol(v)) {
    sparsev <- as.numeric( c(v[, i]) )
    if ( smoother > 0 & !is.na(mask) & FALSE )
      {
      simg = makeImage( mask, sparsev ) %>% iMath("GD",5)
      simg[ mask == 1 ] = sparsev
      simg = smoothImage( simg, sigma = smoother,
        sigmaInPhysicalCoordinates = FALSE )
      sparsevnew = simg[ mask == 1 ]
      sparsev = sparsevnew
      }
    if ( sparam < 0 )
      {
      ord <- order(abs(sparsev))
      }
    else
      {
      sparsev[sparsev < 0] <- 0
      ord <- order(sparsev)
      }
    ord <- rev(ord)
    sparsev[ord[(b+1):length(ord)]] <- 0  # L0 penalty
    if ( ! is.na( mask ) & clustval > 0 )
      {
      sparimg = makeImage( mask, sparsev )
      timg = labelClusters( sparimg, clustval,
        minThresh = 1e-12, maxThresh = Inf )
      if ( sum( timg > 0 ) > 0 ) # otherwise we are degenerates
        {
        timg = thresholdImage( timg, 1, Inf ) * sparimg
        sparsev = timg[ mask > 0.5 ]
        }
      }
    v[, i] <- sparsev
  }
  return( v * mysigns )
}
