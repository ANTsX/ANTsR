#' Label image from Powers points
#'
#' Creates spherical points in the coordinate space of the target image.
#'
#' @param pts input powers points
#' @param mask antsImage mask defining target space
#' @param radius for the points
#' @return antsImage is output
#' @author Avants BB, Duda JT
#' @examples
#'
#' \dontrun{
#' mni <- antsImageRead( getANTsRData( "mni" ) ) %>% getMask()
#' data( "powers_areal_mni_itk", package = "ANTsR", envir = environment() )
#' powersLabels = makePowersPointsImage( powers_areal_mni_itk, mni )
#' # if you have a different target space
#' # pts = antsApplyTransformsToPoints( 3, powers_areal_mni_itk,
#' #          transformlist = concatenatedMaps$toTemplate,
#' #          whichtoinvert = concatenatedMaps$toTemplateInversion )
#' }
#'
#' @export makePowersPointsImage
makePowersPointsImage <- function( pts, mask, radius = 5 )
{
  powersLabels = mask * 0
  nPts = dim(pts)[1]
  rad  = radius
  n = ceiling( rad / antsGetSpacing( mask ) )
  for ( r in 1:nPts) {
    pt = as.numeric(c(pts$x[r], pts$y[r], pts$z[r] ))
    idx = antsTransformPhysicalPointToIndex(mask,pt)
    for ( i in c(-n[1]:n[1]) ) {
      for (j in c(-n[2]:n[2])) {
        for (k in c(-n[3]:n[3])) {
          local = idx + c(i,j,k)
          localpt = antsTransformIndexToPhysicalPoint(mask,local)
          dist = sqrt( sum( (localpt-pt)*(localpt-pt) ))
          inImage = ( prod(idx <= dim(mask))==1) && ( length(which(idx<1)) == 0 )
          if ( (dist <= rad) && ( inImage == TRUE ) ) {
            powersLabels[ local[1], local[2], local[3] ] = pts$ROI[r]
           }
          }
        }
      }
    }
  return( powersLabels )
}
