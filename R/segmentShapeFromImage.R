#' convolution-based shape identification
#'
#' takes an image and a shape and relates the 2nd with the first to yield a
#' feature image - the function is user modifiable but defaults to cor
#'
#' @param img antsImage
#' @param shape to define features
#' @param mask with values 1 or 0
#' @param rad max radius ( optional, not recommended )
#' @param scfun function to apply to create feature image (defaults to cor)
#' @param maskZeroes if TRUE (default), zeroes will not influence the result
#' @return feature image
#' @author Brian B. Avants
#' @keywords shape template
#' @examples
#'
#' set.seed(123)
#' fi<-makeImage(c(20,20),rnorm(400,mean=1,sd=0.1))
#' mask<-getMask(fi,0.8,Inf,0)
#' segs<-kmeansSegmentation( fi ,3 , mask)$segmentation
#' segs[ segs != 1 ]<-0
#' shp<-labelClusters( segs,1)
#' shp[ shp != 1 ]<-0
#' fimg<-segmentShapeFromImage(fi,shp,mask)
#'
#' @export
segmentShapeFromImage <- function(img, shape, mask = NULL, rad = NA, scfun,
  maskZeroes=TRUE ) {
  if (nargs() == 0) {
    print(args(segmentShapeFromImage))
    return(1)
  }
  dim <- img@dimension
  if (is.null(mask)) {
    mask <- getMask(img)
  } else {
    mask = check_ants(mask)
  }
  shapemask<-getMask(shape)
  shape<-cropImage( shape, shapemask )
  shapemask<-cropImage( shapemask, shapemask )
  if (all(is.na(rad))) {
    rad <- round( (( dim( shape ) - 1 ) / 2) )
  }
  eps<-1.e-12 # this is an odd bug : round(dim(img)/2) gives wrong result
  temp<-iMath( as.antsImage(as.array(shape)) , 'PadImage', 1 )
  shapevec<-getNeighborhoodAtVoxel(  temp, round(dim(temp)/2+eps), rad )$values
  temp<-iMath( as.antsImage(as.array(shapemask)) , 'PadImage', 1 )
  shpmskvec<-getNeighborhoodAtVoxel( temp, round(dim(temp)/2+eps), rad )$values
  selector <- ( shpmskvec > eps )
  mat <- getNeighborhoodInMask(img, mask, rad, boundary.condition = "image")
  mat <- antsrimpute( mat )
  if ( maskZeroes )
    {
    shapevec<-shapevec[ selector ]
    mat<-mat[ selector, ]
    }
  if ( ! missing( scfun ) )
    {
    shapecor<-rep( 0.0 , ncol( mat ) )
    for ( kk in 1:ncol( mat ) )
      {
      shapecor[kk] <- scfun( mat[,kk], shapevec )
      }
    } else shapecor<-cor( shapevec, mat )
  # convolve(h2$xt,x2$xt,type="open") # could not get this to work
  shapecor<-antsrimpute( as.numeric( shapecor ) )
  featurei <- makeImage( mask, shapecor )
  return(featurei)
}
