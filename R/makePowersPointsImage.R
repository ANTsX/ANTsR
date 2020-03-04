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
    for ( i in seq(-n[1],n[1],by=0.5) ) {
      for (j in seq(-n[2],n[2],by=0.5)) {
        for (k in seq(-n[3],n[3],by=0.5)) {
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



#' Create label image from physical space points
#'
#' Creates spherical points in the coordinate space of the target image based
#' on the n-dimensional matrix of points that the user supplies. The image
#' defines the dimensionality of the data so if the input image is 3D then
#' the input points should be 2D or 3D.
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
#' powersLabels = makePointsImage( powers_areal_mni_itk[,1:3], mni, radius=3 )
#' }
#'
#' @export makePointsImage
makePointsImage <- function( pts, mask, radius = 5 )
{
  powersLabels = mask * 0
  nPts = dim(pts)[1]
  rad  = radius
  n = ceiling( rad / antsGetSpacing( mask ) )
  dim = mask@dimension
  if ( ncol( pts ) < dim )
    stop( "points dimensionality should match that of images" )
  for ( r in 1:nPts) {
    pt = as.numeric(c(pts[r,1:dim]))
    idx = antsTransformPhysicalPointToIndex(mask,pt)
    for ( i in seq(-n[1],n[1],by=0.5) ) {
      for (j in seq(-n[2],n[2],by=0.5) )  {
        if ( dim == 3 )
          {
          for (k in seq(-n[3],n[3],by=0.5)) {
            local = idx + c(i,j,k)
            localpt = antsTransformIndexToPhysicalPoint(mask,local)
            dist = sqrt( sum( (localpt-pt)*(localpt-pt) ))
            inImage = ( prod(idx <= dim(mask))==1) && ( length(which(idx<1)) == 0 )
            if ( (dist <= rad) && ( inImage == TRUE ) ) {
              if ( powersLabels[ local[1], local[2], local[3] ] < 0.5 )
                powersLabels[ local[1], local[2], local[3] ] = r
             }
            }
          } # if dim == 3
        if ( dim == 2 )
          {
          local = idx + c(i,j)
          localpt = antsTransformIndexToPhysicalPoint(mask,local)
          dist = sqrt( sum( (localpt-pt)*(localpt-pt) ))
          inImage = ( prod(local <= dim(mask))==1) && ( length(which(local<1)) == 0 )
          if ( (dist <= rad) && ( inImage == TRUE ) ) {
              if ( powersLabels[ local[1], local[2] ] < 0.5 )
                powersLabels[ local[1], local[2] ] = r
              }
          } # if dim == 2
        }
      }
    }
  return( powersLabels )
}
