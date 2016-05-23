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
#' @param smooth smooth the input data first by this value
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
#' nvecsSel<-eanatSelect( mat, selectorScale = 1.2, maxNEvec = 4 )
#' esol <- sparseDecom( mat, nvecs = nvecsSel )
#' print(paste("selected", nvecsSel,'pseudo-eigenvectors'))
#' @export eanatSelect
eanatSelect <- function( inmat, mask=NA, cthresh=0, smooth=0,
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
      eseg = eigSeg( mask, ilist,  TRUE, cthresh=cthresh, smooth=smooth  )
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



#' Eigenanatomy with deflation
#'
#' Simplified, low-parameter eigenanatomy implemented with deflation. The
#' algorithm is able to automatically select hidden \code{sparseness}
#' parameters, given the key parameter \code{nvecs}.  The user should select the
#' \code{cthresh} and  \code{smoother} regularization parameters for a given
#' application and also based on observing algorithm behavior when
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
#' @param priors external initialization matrix.
#' @param priorWeight weight on priors in range 0 to 1.
#' @param verbose controls whether computation is silent or not.
#' @return matrix is output, analogous to \code{svd(mat,nu=0,nv=nvecs)}
#' @author Avants BB, Tustison NJ
#' @references Kandel, B. M.; Wang, D. J. J.; Gee, J. C. & Avants, B. B.
#' Eigenanatomy: sparse dimensionality reduction for multi-modal medical
#' image analysis. Methods,  2015, 73, 43-53.
#' PS Dhillon, DA Wolk, SR Das, LH Ungar, JC Gee, BB Avants
#' Subject-specific functional parcellation via Prior Based Eigenanatomy
#' NeuroImage, 2014, 99, 14-27.
#'
#' @examples
#' mat <- matrix(rnorm(2000),ncol=50)
#' nv <- eanatSelect( mat, selectorScale = 1.2 )
#' esol <- eanatDef( mat, nvecs=nv )
#' es2 <- sparseDecom( mat, nvecs = nv )
#' print( paste( "selected", nrow(esol),'pseudo-eigenvectors') )
#' print( mean( abs( cor( mat %*% t(esol)) ) ) ) # what we use to select nvecs
#' \dontrun{
#' networkPriors = getANTsRData("fmrinetworks")
#' ilist = networkPriors$images
#' mni = antsImageRead( getANTsRData("mni") )
#' mnireg = antsRegistration( meanbold*mask, mni, typeofTransform = 'Affine')
#' for ( i in 1:length(ilist) )
#'   ilist[[i]] = antsApplyTransforms( meanbold,ilist[[i]],mnireg$fwdtransform )
#' pr = imageListToMatrix( ilist, cortMask )
#' esol <- eanatDef( boldMat,
#'   nvecs = length(ilist), cortMask, verbose=FALSE,
#'   cthresh = 25, smoother = 0, positivity = TRUE, its=10, priors=pr,
#'   priorWeight=0.15, eps=0.1 )
#' }
#'
#' @seealso \code{\link{eanatSelect}}
#'
#' @export eanatDef
eanatDef <- function( inmat, nvecs=0, mask=NA,
  smoother=0, cthresh=0, its=5, eps=0.1,
  positivity = FALSE, priors=NA, priorWeight=0,
  verbose=FALSE )
{
mat = ( inmat )
if ( !positivity ) keeppos = (-1.0) else keeppos = (1.0)
if ( is.na(mask) ) {
  mask = makeImage( c(3,ncol(mat)+2), voxval=0 )
  mask[ 2, 2:(2+ncol(mat)-1) ] = 1
  }
if ( sum(mask==1) != ncol(mat) ) stop("Mask must match mat")
if ( nvecs >= nrow(mat) ) nvecs = nrow( mat ) - 1
havePriors = TRUE
if ( all( is.na( priors ) ) )
{
if ( nvecs == 0 ) stop("Must set nvecs.  See eanatSelect function.")
havePriors = FALSE
solutionmatrix = t( svd( mat, nu=0, nv=nvecs )$v )
pp1 = mat %*% t( solutionmatrix )
ilist = matrixToImages( solutionmatrix, mask )
eseg = eigSeg( mask, ilist,  TRUE )
solutionmatrix = imageListToMatrix( ilist, mask )
} else {
  nvecs = nrow( priors )
  for ( sol in 1:nrow(priors))
    {
    vec = priors[sol,]
    vec = vec / sqrt( sum( vec * vec ) )
    priors[sol,] = vec
    }
  solutionmatrix = priors
  pp1 = mat %*% t( solutionmatrix )
}
for ( sol in 1:nrow(solutionmatrix))
  {
  vec = solutionmatrix[sol,]
  vec = vec / sqrt( sum( vec * vec ) )
  solutionmatrix[sol,] = vec
  }
sparvals = rep( NA, nvecs )
for ( i in 1:nvecs )
  sparvals[i] = sum( abs(solutionmatrix[i,]) > 0  ) / ncol( mat ) * keeppos
allsols = solutionmatrix[1,] * 0
for ( sol in 1:nrow(solutionmatrix))
  {
  if ( sol == 1 ) rmat = mat else {
    pp = mat %*% t( solutionmatrix )
    rmat = residuals( lm( mat ~ pp[ ,1:(sol-1)] ) )
    }
  # now do projected stochastic gradient descent
  for ( i in 1:its )
    {
    vec = solutionmatrix[sol,]
    vec = vec / sqrt( sum( vec * vec ) ) # this is initial vector
    grad = vec * 0
#    grad = .bootSmooth( rmat, vec, nboot=0 )
    grad = t( rmat ) %*% ( rmat %*% vec )
    grad = grad / sqrt( sum( grad * grad ) )
    if ( havePriors )
      {
      tempp = ( priors[sol,] )
      grad2 = t( tempp ) * ( tempp * vec ) # may accidentally zero out
      if ( max( abs( grad2 ) ) == 0 ) {
        print(paste("zeroed!!!!",sol))
        grad2 = t(priors[sol,])
        }
      grad2 = grad2 / sqrt( sum( grad2 * grad2 ) )
      grad = grad * ( 1 - priorWeight) + t(grad2) * priorWeight
      grad = grad / sqrt( sum( grad * grad ) )
      }
#    if ( havePriors )
#      grad = grad * ( 1 - priorWeight) + priors[sol,] * priorWeight
    vec = vec + grad * eps
    vec = .hyperButt( vec, sparvals[sol], mask=mask,
      smoother=smoother, clustval=cthresh, verbose=verbose )
    vec = vec / sqrt( sum( vec * vec ) )
    rq = sum( vec * ( t(mat) %*% ( mat %*% vec ) ) )
    if ( verbose ) print( rq )
    solutionmatrix[sol,]=vec
    }
  allsols = allsols + abs( vec )
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



.bootSmooth <- function( rmat, vec,  nboot )
  {
  if ( nboot == 0 )
    {
    grad = t( rmat ) %*% ( rmat %*% vec )
    grad = grad / sqrt( sum( grad * grad ) )
    return( grad )
    }
  else
    {
    sgrad = vec
    for ( i in 1:nboot )
      {
      n = nrow(rmat)
      myboot = sample( 1:n, replace=FALSE, size=round(0.5*n) ) # 50% drop
      bootmat = rmat[myboot,]
      lg = t( bootmat ) %*% ( bootmat %*% sgrad )
      lg = lg / sqrt( sum( lg * lg ) )
      sgrad = sgrad + lg
      sgrad = ( sgrad / sqrt( sum( sgrad * sgrad ) ) )
      }
    return( sgrad[] )
    }
}


.hyperButt <- function( vin, sparam, mask = NA,
  smoother=0, clustval = 0, verbose = F )
  {
    vin = matrix( vin, ncol=1 )
    if ( any( is.na( vin ) ) ) vin = antsrimpute( vin )
    if ( max(vin) <= 0 )       vin = vin *(-1)
    if ( sum(vin<0) > sum(vin>0) ) vin = vin *(-1)
    if (abs(sparam) >= 1)      return(vin)
    if (nrow(vin) < ncol(vin)) v <- t(vin) else v <- vin
    sparsev <- as.numeric( c(v[, 1]) )
    sparResolution = ( 1.0 / length(sparsev) ) * 2
    if ( sparam > 0 )
      {
      sparsev[sparsev < 0] <- 0
      }
    mysigns = sign( sparsev )
#    print( "rangein" )
#    print( range(sparsev) )
    # smooth first
    if ( smoother > 0 & !is.na(mask)  )
      {
      simg = makeImage( mask, sparsev ) # %>% iMath("GD",5)
      simg[ mask == 1 ] = sparsev
      simg = smoothImage( simg, sigma = smoother,
        sigmaInPhysicalCoordinates = FALSE )
      sparsevnew = simg[ mask == 1 ]
      sparsev[ ] = sparsevnew[ ]
      }
    sparsenessThresh = max( abs( sparsev ) )
    optinterval = c( -1.0 * sparsenessThresh, sparsenessThresh )
    # this is the key geometric operation
    operateOnVec <- function( myvec, s, locth )
      {
      cursparvec = myvec * 0
      if ( ! is.na( mask ) & locth > 0 )
        {
        sparimg = makeImage( mask, myvec )
        timg = labelClusters( abs(sparimg), clustval,
          minThresh = s, maxThresh = Inf )
        if ( sum( timg > 0 ) > 0 )
          {
          timg = thresholdImage( timg, 1, Inf ) * sparimg
          cursparvec = timg[ mask > 0.5 ]
#          selector = cursparvec == 0
#          cursparvec[ selector ] = myvec[ selector ] * 0.9
          } else {
#           back up plan - return largest component
            timg = labelClusters( abs( sparimg ), 2,
              minThresh = s, maxThresh = Inf )
            timg = thresholdImage( timg, 1, 1 ) * sparimg
            cursparvec = timg[ mask > 0.5 ]
          }
        }
      else
        {
        cursparvec = myvec
        cursparvec[ abs(myvec) < s ] = 0
        }
        if ( smoother > 0 & !is.na(mask) & FALSE )
          {
          simg = makeImage( mask, cursparvec ) # %>% iMath("GD",5)
          simg[ mask == 1 ] = cursparvec
          simg = smoothImage( simg, sigma = smoother,
            sigmaInPhysicalCoordinates = FALSE )
          cursparvec = simg[ mask == 1 ]
          }
        return( cursparvec )
      }
    # we wish to call optimize with a function that returns a value
    # based on the difference between the current and target sparval
    # the input argument is sparsenessThresh
    myoptf <- function( s , sparsevIn, locth = 0 )
      {
      cursparvec = operateOnVec( sparsevIn, s, locth=locth )
      myspar = sum( abs( cursparvec) >  0 ) / length( sparsevIn )
      testspar = abs( abs(myspar) - abs(sparam) )
      if ( myspar == 0 ) {
        testspar = Inf
        }
#      print( paste( "s", s, "myspar", myspar, "goal", sparam ,"testspar", testspar ) )
      return( testspar )
      }
    smin = optimize( myoptf, interval = range( sparsev ),
        tol = sparResolution, sparsevIn=sparsev, locth = 0 )$minimum
    if ( clustval > 0 )
      {
      optinterval2 = range( sparsev )
      optinterval2[ 1 ] = smin * (-1)
      optinterval2[ 2 ] = smin
      smin = optimize( myoptf, interval = optinterval2,
        lower = min(optinterval2), upper = max(optinterval2),
        tol = sparResolution, sparsevIn=sparsev, locth = clustval )$minimum
      }
    temp = operateOnVec( sparsev, smin, locth=clustval  )
    myspar = sum( abs( temp) >  0 ) / length( temp )
    if ( max(abs(temp)) == 0 )
      {
      if ( verbose ) print("zeroed too much")
      temp = operateOnVec( sparsev, smin, locth=0  )
      myspar = sum( abs( temp) >  0 ) / length( temp )
      }
    v[, 1] <- temp
#    if ( verbose )
#      print( paste( "tar", sparam, "got", myspar, "mx", max(abs(sparsev)), "smin", smin ))
    v = v * mysigns
    return( v )
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
