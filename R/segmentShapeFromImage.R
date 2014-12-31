#' @name segmentShapeFromImage
#' @title convolution-based shape identification
#' @description  takes an image and a shape and concvoles the 2nd with the first to yield a feature image
#' @usage  featimg<-segmentShapeFromImage( img, shape, mask )
#' @param img antsImage
#' @param shape to define features
#' @param mask  with values 1 or 0
#' @param rad  max radius (optional)
#' @param scfun  function to apply to feature image (optional eg abs)
#' @return feature image
#' @author Brian B. Avants
#' @keywords shape template
#' @examples
#' set.seed(123)
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' mask<-getMask(fi)
#' segs<-Atropos( d = 2, a = fi, m = "[0.2,1x1]",c = "[5,0]",  i = "kmeans[3]", x = mask)$segmentation
#' segs[ segs != 1 ]<-0
#' shp<-labelClusters( antsImageClone(segs,'float'),10)
#' shp[ shp != 7 ]<-0
#' fimg<-segmentShapeFromImage(fi,shp,mask)
#' plotANTsImage(fi,func=list(fimg),
#'   thresh=paste(mean(fimg)+sd(fimg)*2,
#'   max(fimg),sep='x'))
segmentShapeFromImage<-function( img, shape,
  mask=NA, rad=NA, scfun )
{
  if (nargs() == 0) {
    print(args(segmentShapeFromImage))
    return(1)
  }
  dim<-img@dimension
  if ( is.na(mask) ) mask<-getMask(img)
  if ( all( is.na( rad ) ) ) {
    # shapearr<-as.array(shape)
    # ww<-which( shapearr > 0, arr.ind=TRUE)
    shapesum<-sum(shape>0)
    rd<-round( shapesum^(1.0/dim)*0.5 )+1
    rad<-rep(rd,dim)
    print(paste("computed r",rd))
  }
  shapemask<-getMask(shape,0.5,Inf,0)
  mat<-antsGetNeighborhoodMatrix(shape, shapemask,
    rad, boundary.condition='image')
  mat<-antsrimpute(mat)
  matsums<-colSums(mat)
  shapevec<-rowMeans( mat[ , rep(which.max( matsums ),2)    ] )
  shapevec<-shapevec/max(shapevec)
  mat<-antsGetNeighborhoodMatrix(img, mask,
    rad,boundary.condition='image')
    mat<-antsrimpute(mat)
  shapecor<-cor( mat, shapevec )
  if ( ! missing(scfun) )
    shapecor<-scfun( shapecor )
#  refvec<-shape[ mask == 1 ]
#  corinshape<-mean( refvec * shapecor )
#  if ( corinshape < 0  )
#    shapecor<-shapecor*(-1)
  featurei<-makeImage(mask, shapecor )
  return(featurei)
}
