#' Shape analysis of deformation fields based on PCA
#'
#' Converts a set of deformation fields to a matrix in order to
#' enable various forms of PCA.  Returns the components of shape
#' variability and variance explained.  Can optionally employ
#' a few different types of decomposition.
#'
#' @param x list containing deformation fields
#' @param mask mask to apply to the deformation field
#' @param k rank to use
#' @param pcaOption currently only PCA and randPCA,
#' the latter being much faster.
#' @return matrix is output
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
#' dpca = deformationFieldPCA( x, mask )
#' warpTx = antsrTransformFromDisplacementField( dpca$pcaWarps[[1]] )
#' warped = applyAntsrTransform( warpTx, data = img1, reference = img1)
#'
#' @export deformationFieldPCA
deformationFieldPCA <- function(
  x,
  mask,
  k = NA,
  pcaOption = "PCA") {
n = length( x )
dim = mask@dimension
p   = sum( mask >= 1 )
vecmat = matrix( nrow = n, ncol = p*dim )
for ( i in 1:n )
  {
  temp = as.array( x[[ i ]] )
  vv = 0
  for ( d in 1:dim )
    {
    tempimg = as.antsImage( temp[d,,] )
    a = antsCopyImageInfo( mask, tempimg )
    if ( length( vv ) == 1 ) vv = tempimg[ mask == 1 ] else
      vv = c( vv, tempimg[ mask == 1 ] )
    }
  vecmat[ i, ] = vv
  }
  if ( is.numeric( pcaOption ) ) {
    pcak = pcaOption
    pcaOption = "kPCA"
  }
  if ( is.na( k ) ) k = nrow( vecmat ) - 1
  cx   = sweep( vecmat, 2, colMeans(vecmat), "-")
  if ( pcaOption == "randPCA" ) {
    if ( ! usePkg( "rsvd" ) ) stop("please install rsvd")
    vpca = rsvd::rsvd( cx, k )
    } else if ( pcaOption == "kPCA" ) {
      if ( ! usePkg( "irlba" ) ) stop("please install irlba")
      tempdistmat = sparseDistanceMatrix( cx, pcak, kmetric='cov' )
      vpca = irlba::irlba( tempdistmat, nu=k, nv=k )
    }else {
      vpca = svd(cx, nv=k, nu=k )
    }
  #  plot( vpca$u[, 1], vpca$u[, 2] )
  # now convert the vectors back to warps
  vectortowarp <- function( v, mask ) {
    dd = mask@dimension
    p = sum( mask >= 1 )
    if ( length( vv ) != dd*p ) stop("dimensions do not match")
    mylist = list( )
    maxn = (dd*p)
    myinds = seq( from=1, to=maxn, by=(p-1) )
    for ( k in 1:( length( myinds )-1 ) )
      {
      temp = antsImageClone( mask )
      temp[ mask == 1 ] = v[ myinds[k]:(myinds[k+1])]
      mylist[[ k ]] = temp
      }
    vecimg = mergeChannels( mylist )
    a=antsCopyImageInfo( mask, vecimg )
    return( vecimg * (-1) ) # invert the field
  }
  pcaWarps = list( )
  for ( i in 1:k )
    {
    pcaWarps[[ i ]] = vectortowarp( vpca$v[,i], mask )
    }
  return( list( pca = vpca, pcaWarps=pcaWarps ) )
}
