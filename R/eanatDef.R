#' Eigenanatomy with deflation
#'
#' Simplified, low-parameter eigenanatomy implemented with deflation. The
#' algorithm is able to automatically select hidden \code{sparseness}
#' parameters, given the key parameter \code{nvecs}.  The user should select the
#' \code{cthresh} and  \code{smoother} regularization parameters for his or her #' application and also based on observing algorithm behavior when
#' \code{verbose=TRUE}.
#'
#' @param inmat input matrix
#' @param nvecs number of eigenanatomy vectors to compute. see
#' \code{eanatSelect} for a method to compute an optimal \code{nvecs} value.
#' @param mask input mask, must match matrix
#' @param smoother regularization parameter, typically 0 or 0.5, in voxels
#' @param cthresh remove isolated voxel islands of size below this value
#' @param its number of iterations
#' @param eps gradient descent parameter
#' @param positivity return unsigned eigenanatomy vectors
#' @param verbose controls whether computation is silent or not.
#' @return matrix is output, analogous to \code{svd(mat,nu=0,nv=nvecs)}
#' @author Avants BB, Tustison NJ
#' @references Kandel, B. M.; Wang, D. J. J.; Gee, J. C. & Avants, B. B.
#' Eigenanatomy: sparse dimensionality reduction for multi-modal medical
#' image analysis. Methods,  2015, 73, 43-53.
#'
#' @examples
#' mat <- matrix(rnorm(2000),ncol=50)
#' nv <- eanatSelect( mat, selectorScale = 1.2 )
#' esol <- eanatDef( mat, nvecs=nv )
#' es2 <- sparseDecom( mat, nvecs = nv )
#' print( paste( "selected", nrow(esol),'pseudo-eigenvectors') )
#' print( mean( abs( cor( mat %*% t(esol)) ) ) ) # what we use to select nvecs
#'
#' @seealso \code{\link{eanatSelect}}
#'
#' @export eanatDef
eanatDef <- function( inmat, nvecs, mask=NA,
  smoother=0, cthresh=0, its=5, eps=0.1,
  positivity = FALSE, verbose=FALSE )
{
mat = scale( inmat )
if ( !positivity ) keeppos = (-1.0) else keeppos = (1.0)
if ( is.na(mask) ) {
  mask = makeImage( c(3,ncol(mat)+2), voxval=0 )
  mask[ 2, 2:(2+ncol(mat)-1) ] = 1
  }
if ( sum(mask==1) != ncol(mat) ) stop("Mask must match mat")
if ( missing(nvecs) ) stop("Must set nvecs.  See eanatSelect function.")
if ( nvecs >= nrow(mat) ) nvecs = nrow( mat ) - 1
solutionmatrix = t( svd( mat, nu=0, nv=nvecs )$v )
pp1 = mat %*% t( solutionmatrix )
ilist = matrixToImages( solutionmatrix, mask )
eseg = eigSeg( mask, ilist,  TRUE )
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





#' Selection of n eigenvectors and sparsity for eigenanatomy
#'
#' The algorithm automatically selects the key \code{nvecs} and hidden
#' \code{sparseness} parameters.  The user should select the \code{cthresh}
#' regularization parameters for his or her application. The principle used
#' here is that we want few but sparse pseudo-eigenvectors that are minimally
#' correlated in row-space. true left and right eigenvectors are uncorrelated
#' in both row and column (left and right eigenvector) spaces, but this is not
#' the case when we impose sparsity.
#'
#' @param inmat input matrix
#' @param mask input mask, must match matrix
#' @param cthresh remove isolated voxel islands of size below this value
#' @param maxNEvec integer that, if set greater than zero, indicates that we use
#' a low-rank approximation to the input matrix before proceeding to eanat.
#' this value should be greater than \code{nvecs}
#' @param selectorScale influences automatic selection of \code{nvecs} and tries
#' to find the knee in the correlation plot. This parameter produces fewer,
#' less sparse eigenanatomy pseudo-eigenvectors as its value increases.  Its
#' minimum value is 1 and a reasonable range is between 1 and 2.  The user
#' should look at the plot produced when verbosity is turned on.
#' @param verbose controls whether computation is silent or not.
#' @return nvecs is output, analogous to \code{nvecs} in
#' \code{svd(mat,nu=0,nv=nvecs)}
#' @author Avants BB, Tustison NJ
#'
#' @examples
#' mat <- matrix(rnorm(2000),ncol=50)
#' nvecsSel<-eanatSelect( mat, selectorScale = 1.2 )
#' esol <- sparseDecom( mat, nvecs = nvecsSel )
#' print(paste("selected", nvecsSel,'pseudo-eigenvectors'))
#' @export eanatSelect
eanatSelect <- function( inmat, mask=NA, cthresh=0,
  maxNEvec = 0, selectorScale=1.1, verbose=FALSE )
{
mat = scale( inmat )
if ( is.na(mask) ) {
  mask = makeImage( c(3,ncol(mat)+2), voxval=0 )
  mask[ 2, 2:(2+ncol(mat)-1) ] = 1
  }
if ( sum(mask==1) != ncol(mat) ) stop("Mask must match mat")
if ( selectorScale < 1 ) selectorScale = 1.1
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
return( nvecs )
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
    if ( TRUE )
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
      n = nrow(rmat)
      myboot = sample( 1:n, replace=TRUE, size=round(0.8*n) )
      bootmat = rmat[myboot,]
      lg = t( bootmat ) %*% ( bootmat %*% vec )
      lg = lg / sqrt( sum( lg * lg ) )
      sgrad = sgrad + grad
      }
    grad[ ] = ( sgrad / sqrt( sum( sgrad * sgrad ) ) )[ ]
    return( grad )
    }
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
    if ( smoother > 0 & !is.na(mask)  )
      {
      simg = makeImage( mask, sparsev ) %>% iMath("GD",5)
      simg[ mask == 1 ] = sparsev
      simg = smoothImage( simg, sigma = smoother,
        sigmaInPhysicalCoordinates = FALSE )
      sparsevnew = simg[ mask == 1 ]
      sparsev[ ] = sparsevnew[ ]
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
          minThresh = 1e-9, maxThresh = Inf )
        cvl = round( clustval * 0.95 )
        while ( (sum( timg > 0 ) == 0 ) & cvl > 1 )# otherwise we are degenerates
          {
          timg = labelClusters( sparimg, cvl,
            minThresh = 1e-9, maxThresh = Inf )
          cvl = round( cvl * 0.95 )
          }
        if ( sum( timg > 0 ) > 0 )
          {
          timg = thresholdImage( timg, 1, Inf ) * sparimg
          sparsev = timg[ mask > 0.5 ]
          }
      }
    v[, i] <- sparsev
  }
  return( v * mysigns )
}



#' Test eigenanatomy in order
#'
#' This tests each eigenanatomy region in order invisibly to the user and
#' stops when testing stops conserving power.  The algorithm returns NA if
#' there is no good testing procedure, given the statistical target. The basic
#' idea is to test the variables that explain the most variance first and
#' continue testing as long as (a) there is no significant relationship yet
#' found or (b) the existing significant relationship remains significant,
#' given the correction for multiple comparisons.
#'
#' @param mymdl input model from \code{lm} with eigenanatomy on left
#' @param myvar name of variable in model to test
#' @param sigthresh significance threshold, for example 0.05
#' @param method a method for \code{p.adjust}
#' @return nvecs is output, analogous to \code{nvecs} in
#' \code{svd(mat,nu=0,nv=nvecs)}
#' @author Avants BB, Tustison NJ
#' @references Avants BB, Hackman D, LM Betancourt, GM Lawson, H Hurt, MJ Farah #' Relation of Childhood Home Environment to Cortical Thickness in
#' Late Adolescence: Specificity of Experience and Timing, PloS one, 2015
#'
#' @examples
#' mat <- matrix(rnorm(300),ncol=3)
#' n = nrow(mat)
#' g = factor( c(rep(1,n/2),rep(0,n/2)) )
#' mydf = data.frame( gen=g, a=rnorm(n))
#' mymdl<-lm( mat ~ a + g , data=mydf )
#' nv = testEanat( mymdl, myvar="g1" )
#' mat[1:(n/2),3] = mat[1:(n/2),3] + 2
#' mymdl<-lm( mat ~ a + g , data=mydf )
#' nv = testEanat( mymdl, myvar="g1" )
#'
#' @export testEanat
testEanat <- function( mymdl, myvar, sigthresh=0.05, method='BH' )
  {
  bmymdl = bigLMStats( mymdl )
  pvec = bmymdl$beta.pval[ myvar, ]
  bestval = 1
  bestnsig = 0
  for ( k in 1:ncol( bmymdl$beta.pval ) )
    {
    qvec = p.adjust( pvec[1:k], method )
    nsig = sum( qvec <= sigthresh )
    if ( nsig >= bestnsig )
      {
      bestnsig = nsig
      bestval = k
      }
    }
  if ( bestnsig == 0 ) return(NA)
  return( bestval )
  }
