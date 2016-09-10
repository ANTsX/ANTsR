#' Shape analysis of multichannel images based on PCA
#'
#' Converts a set of multichannel images (e.g. deformation fields ) to a matrix
#' to enable various forms of PCA.  Returns the components of shape variability
#' and variance explained.  May employ different decomposition methods (WIP).
#'
#' @param x list containing multichannel images all of the same size
#' @param mask mask to apply to the multichannel images
#' @param k rank to use
#' @param pcaOption currently only PCA and randPCA, latter being much faster.
#' We also allow fastICA.
#' @param auxiliaryModality if you pass this matrix, then will do CCA.  This
#' will only work with one option.
#' @param center subtract the mean column vector.
#' @param sigma parameter for kernel PCA.
#' @param verbose produces more explanatory output.
#' @return list of the pca output and conversion to multichannel images
#' @author Avants BB
#' @examples
#'
#' img1 = antsImageRead( getANTsRData( "r16" ) ) %>%
#'   resampleImage( c(4,4) )
#' img2 = antsImageRead( getANTsRData( "r64" ) ) %>%
#'   resampleImage( c(4,4) )
#' img3 = antsImageRead( getANTsRData( "r27" ) ) %>%
#'   resampleImage( c(4,4) )
#' img4 = antsImageRead( getANTsRData( "r30" ) ) %>%
#'   resampleImage( c(4,4) )
#' reg1 = antsRegistration( img1, img2, 'SyN' )
#' reg2 = antsRegistration( img1, img3, 'SyN' )
#' reg3 = antsRegistration( img1, img4, 'SyN' )
#' w1 = antsImageRead( reg1$fwdtransforms[1] )
#' w2 = antsImageRead( reg2$fwdtransforms[1] )
#' w3 = antsImageRead( reg3$fwdtransforms[1] )
#' mask = getMask( img1 )
#' x = list( w1, w2, w3 )
#' dpca = multichannelPCA( x, mask )
#' warpTx = antsrTransformFromDisplacementField( dpca$pcaWarps[[1]] )
#' warped = applyAntsrTransform( warpTx, data = img1, reference = img1)
#'
#' @export multichannelPCA
multichannelPCA <- function(
  x,
  mask,
  k = NA,
  pcaOption = "PCA",
  auxiliaryModality,
  center = TRUE,
  sigma = NA,
  verbose = FALSE ) {
n = length( x )
idim = mask@dimension
p   = sum( mask >= 1 )
# check the size of the inputs matches the mask
for ( i in 1:n )
  if ( !all( dim( x[[n]] ) == dim(mask)  ) )
    stop( paste("dimensionality of input number",i,
    "does not match the mask dimensionality."))
nchannels = x[[ 1 ]]@components
vecmat = matrix( nrow = n, ncol = p*nchannels )
for ( i in 1:n )
  {
  vecmat[ i, ] = multichanneltovector( x[[ i ]] , mask )
  }
  if ( is.numeric( pcaOption ) ) {
    pcak = pcaOption
    pcaOption = "kPCA"
  }
  if ( verbose ) print( paste( "begin PCA option",pcaOption) )
  if ( is.na( k ) ) k = nrow( vecmat ) - 1
  if ( center ) cx   = sweep( vecmat, 2, colMeans(vecmat), "-") else cx=vecmat
  if ( pcaOption == "randPCA" ) {
    if ( ! usePkg( "rsvd" ) ) stop("please install rsvd")
    vpca = rsvd::rsvd( cx, k )
    } else if ( pcaOption == "kPCA" ) {
      kpcaopt = 'cov'
      if ( ! is.na( sigma ) ) kpcaopt = 'euc'
      if ( ! usePkg( "irlba" ) ) stop("please install irlba")
      if ( missing( "auxiliaryModality") )
        tempdistmat = sparseDistanceMatrix( cx, pcak, kmetric=kpcaopt )
      if ( !missing( "auxiliaryModality") )
        {
        if ( center ) cy = sweep( auxiliaryModality, 2, colMeans(auxiliaryModality), "-")
        if ( !center ) cy = auxiliaryModality
        tempdistmat = sparseDistanceMatrixXY( cy, cx, pcak, kmetric=kpcaopt )
        }
      if ( ! is.na( sigma ) )
        tempdistmat = -1.0 * ( tempdistmat^2 ) / ( 2.0 * sigma ^2 )
      vpca = irlba::irlba( tempdistmat, nu=k, nv=k )
    } else if ( pcaOption == "fastICA" ) {
      if ( ! usePkg( "fastICA" ) ) stop("please install irlba")
      tempica = fastICA( t( cx ), k )
      vpca = list( d=NA,  u=NA,
        v=( tempica$S ) )
    } else if ( pcaOption == "eanat" ) {
      # FIXME - implement mask for regularization
      # need to bind mask in proper order to make
      # regularization work
      arr = abind::abind( as.array( mask ), as.array( mask ), along=idim )
      if ( idim == 3 )
        arr = abind::abind( arr,  as.array( mask ), along=idim )
      maska = as.antsImage( arr )
      a = antsCopyImageInfo( mask, maska )
#      eanat = sparseDecom( cx, inmask=maska, sparseness = 1.0/k, nvecs=k,
#        cthresh=10, smooth=0.5, verbose=TRUE )
#      vpca = list( d=eanat$varex,  u=eanat$umatrix,
#       v=t(eanat$eigenanatomyimages ) )
      eanatD = eanatDef( cx, mask = maska, smoother = mean(antsGetSpacing(mask)),
        positivity=TRUE, nvecs = k, cthresh=0, verbose=TRUE )
      vpca = list( d=NA,  u=NA, v=t( eanatD ) )
      k = ncol( vpca$v )
    } else {
      vpca = svd(cx, nv=k, nu=k )
    }
  if ( !verbose ) { rm( vecmat ); vecmat=NA }
  if ( verbose ) {
    print( paste( "convert back to multichannel" ) )
#    print( dim( vpca$v ) )
#    print( dim(mask) )
#    print( sum( mask ) )
#    print( k )
    }
  # now convert the vectors back to warps
  pcaWarps = list( )
  for ( i in 1:k )
    {
    pcaWarps[[ i ]] = vectortomultichannel( vpca$v[,i], mask )
    }
  datatopcacorrs = NA
  mylms = NA
  if ( verbose ) print( paste( "regression and correlation" ) )
  if ( verbose )
    {
    datatopcacorrs = cor( t( vecmat ) , vpca$v )
    # can also explore regressing the basis against the data
    mydf = data.frame( vpca$v )
    mylms = summary(lm( t( vecmat ) ~ .,data=mydf))
    }
  gc( )
  return( list(
    pca = vpca,
    pcaWarps = pcaWarps,
    vecmat = vecmat,
    datatopcacorrs = datatopcacorrs,
    mylms = mylms ) )
}


#' Convert multichannel \code{antsImage} to a vector.
#'
#' Employs a mask to flatten each channel of a multi-channel image into a
#' contiguous piece of a vector.
#'
#' @param multichannelimage input multichannel image
#' @param mask mask of same dimensionality as multichannelimage
#' @return vector is output
#' @author Avants BB
#' @examples
#'
#' # see vectortomultichannel
#'
#' @export multichanneltovector
multichanneltovector <- function( multichannelimage, mask )
{
  dd = mask@dimension
  p = sum( mask >= 1 )
  nchannels = multichannelimage@components
  temp = splitChannels( multichannelimage )
  maxn = (nchannels*p)
  myinds = seq( from=1, to=maxn, by=(p) )
  myinds[ nchannels+1 ] = maxn
  v = rep( 0, maxn )
  for ( k in 1:nchannels ) {
    maxind = (myinds[k+1]-1)
    if ( k == nchannels ) maxind = maxn
    locinds = myinds[k]: maxind
    v[ locinds ] = temp[[k]][ mask >= 1 ]
    }
  rm( temp )
  gc()
  return( v )
}


#' Convert vector to multichannel \code{antsImage}.
#'
#' Converts a vector of size n-entries in mask times number of channels to
#' a multi-channel image with the same dimensionality as the mask.
#'
#' @param v input multichannel vector
#' @param mask mask of same dimensionality as multichannelimage
#' @return multichannelimage is output
#' @author Avants BB
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16") ) %>%
#'   resampleImage(c(60,60),1,0)
#' mi <- antsImageRead(getANTsRData("r64") ) %>%
#'   resampleImage(c(60,60),1,0)
#' mytx <- antsRegistration(fixed=fi, moving=mi, typeofTransform = c('SyN') )
#' mcimg = antsImageRead( mytx$fwd[1] )
#' msk = getMask( fi )
#' vv=multichanneltovector(mcimg,msk)
#' mcimg2=vectortomultichannel( vv, msk )
#' vv2=multichanneltovector(mcimg2,msk)
#' cor.test(vv2,vv)
#' stopifnot( all( mcimg2[30,30] == mcimg[30,30] ) )
#'
#' @export vectortomultichannel
vectortomultichannel <- function( v, mask ) {
  dd = mask@dimension
  p = sum( mask >= 1 )
  nchannels = round( length( v ) / p )
  if ( length( v ) != (nchannels*p) ) stop("dimensions do not match")
  maxn = (nchannels*p)
  myinds = seq( from=1, to=maxn, by=(p) )
  myinds[ nchannels+1 ] = maxn
  mylist = list( )
  for ( k in 1:nchannels )
    {
    maxind = (myinds[k+1]-1)
    if ( k == nchannels ) maxind = maxn
    locinds = myinds[k]: maxind
    temp = antsImageClone( mask )
    temp[ mask >= 1 ] = v[ locinds ]
    mylist[[ k ]] = temp
    }
  vecimg = mergeChannels( mylist )
  rm( temp, mylist )
  gc()
  return( vecimg )
}
