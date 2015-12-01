#' REHO
#'
#' regional homogeneity for a time series image
#'
#' @param rsfmri antsImage usually 4D
#' @param mask antsImage binary mask
#' @param shifter size of neighborhood
#' @param method one of kendall, spearman or pearson
#' @param maxDistance maximum distance from center voxel
#'
#' @return antsImage is output
#' @author Tustison, N.
#' @references Zang, Y., Jiang, T., Lu, Y., He, Y., Tian, L., 2004. Regional
#' homogeneity approach to fMRI data analysis. Neuroimage 22, 394-400.
#' @examples
#' \dontrun{
#' rehoimg = reho( rsfmri )
#' }
#' @export reho
reho <- function(  rsfmri, mask, shifter=1,
  method = "kendall",
  maxDistance = 1.1 )
{
if ( !usePkg("magic") ) stop("Need magic package")
if ( missing( mask ) )
  {
  mask = getMask( getAverageOfTimeSeries( rsfmri ) )
  }
ndim = length( dim( rsfmri ) )
myshifts <- getNeighborhoodInMask( mask, mask, rep(shifter,ndim-1),
  spatial.info=TRUE )$offsets
mydistance = rep( 0.0, nrow( myshifts ) )
for ( k in 1:nrow(myshifts) )
  {
  mydistance[ k ] = sqrt(  sum( as.numeric(myshifts[k,])^2.0  ) )
  }
basemat = timeseries2matrix( rsfmri, mask )
corimg = antsImageClone( mask )
corvec = mask[ mask == 1 ] * 0
ct = 0
for ( k in 1:nrow(myshifts) )
  {
  ss = myshifts[k,]
  if (  ( mydistance[k] <  maxDistance ) & ( mydistance[k] > 1.e-9 ) )
    {
    locshiftarr = c( ss, 0 )
    rsfmris = antsCopyImageInfo( rsfmri, as.antsImage( magic::ashift( as.array( rsfmri ) , locshiftarr ) ) )
    mshift = timeseries2matrix( rsfmris, mask )
    for ( i in 1:ncol(basemat) )
      {
      temp = cor.test( basemat[,i], mshift[,i]  , method=method, alternative = "t" )
      corvec[ i ] = corvec[ i ] + temp$estimate
      }
    print( locshiftarr )
    ct=ct+1
    }
  }
  corimg[ mask == 1 ] = corvec / as.numeric( ct )
  return( corimg )
}
