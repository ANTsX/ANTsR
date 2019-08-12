#' Normalize (modified) probability images
#'
#' Probability image lists should sum to one at every voxel. Smoothing or other
#' common operations may break this constraint.  This function allows one to
#' easily ensure a list of proability images sums to one everywhere.  It also
#' allows one prioritize specific images in the probability set such that its
#' probability is maintained while others are normalized with respect to the
#' prioritized value.  Will also set values less than zero, to zero.
#'
#' @param probs list of images
#' @param mask image
#' @param k which image, if any, to prioritize
#' @return list is output
#' @author Avants BB
#' @examples
#'
#' mat <- t( matrix( rnorm(4000),ncol=10) )
#' mask = makeImage( c(20,20), 1 )
#' plist = matrixToImages( mat, mask )
#' rplist<-renormalizeProbabilityImages( plist, mask )
#' rplist2<-renormalizeProbabilityImages( plist, mask , 2 )
#'
#' @export renormalizeProbabilityImages
renormalizeProbabilityImages <- function( probs, mask, k=NA )
{
  if ( is.na( k ) )
  {
    np = length( probs )
    pmat=imageListToMatrix( probs, mask )
    pmat[ pmat < 0 ] = 0
    prsums=colSums( pmat ) # i=26 for testing
    for ( i in 1:ncol(pmat) ) {
      if ( prsums[i] > 0 )
        pmat[,i]=pmat[,i]/prsums[i]
      else pmat[,i]=1.0/np
    }
    return( matrixToImages( pmat, mask ) )
  }
  indvec=1:length(probs)
  ink= indvec %in% k
  ncsf=indvec[ !ink ]
  pmat=imageListToMatrix( probs, mask )
  pmat[ pmat < 0 ] = 0
  newcsfvec = pmat[k,]
  for ( i in 1:ncol(pmat) ) {
    colvec=pmat[,i]
    ncsfsum=sum( colvec[ncsf] )
    csfval=newcsfvec[i]
    colvec[k]=csfval # the fix is in!
    if ( ncsfsum > 0 )
      colvec[ncsf]=colvec[ncsf]/ncsfsum*(1.0-csfval)
    else colvec[k]=1
    pmat[,i]=colvec
  }
  return( matrixToImages( pmat, mask ) )
}
