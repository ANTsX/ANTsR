#' convolution-based shape identification
#'
#' takes an image and a shape and concvoles the 2nd with the first to yield a
#' feature image
#'
#'
#' @param img antsImage
#' @param shape to define features
#' @param mask with values 1 or 0
#' @param rad max radius (optional)
#' @param scfun function to apply to feature image (optional eg abs)
#' @return feature image
#' @author Brian B. Avants
#' @keywords shape template
#' @examples
#'
#' set.seed(123)
#' fi<-makeImage(c(20,20),rnorm(400,mean=1,sd=0.1))
#' # fi<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask(fi,0.8,Inf,0)
#' segs<-kmeansSegmentation( fi ,3 , mask)$segmentation
#' segs[ segs != 1 ]<-0
#' shp<-labelClusters( segs,1)
#' shp[ shp != 1 ]<-0
#' fimg<-segmentShapeFromImage(fi,shp,mask)
#' \dontrun{
#' plot(fimg)
#' }
#' @export segmentShapeFromImage
segmentShapeFromImage <- function(img, shape, mask = NA, rad = NA, scfun) {
  if (nargs() == 0) {
    print(args(segmentShapeFromImage))
    return(1)
  }
  dim <- img@dimension
  if (is.na(mask))
    mask <- getMask(img)
  if (all(is.na(rad))) {
    # shapearr<-as.array(shape) ww<-which( shapearr > 0, arr.ind=TRUE)
    shapesum <- sum(shape > 0)
    rd <- round(shapesum^(1/dim) * 0.5) + 1
    rad <- rep(rd, dim)
  }
  shapemask <- getMask(shape, 0.05, Inf, 0)
  mat <- antsGetNeighborhoodMatrix(shape, shapemask, rad, boundary.condition = "image")
  mat <- antsrimpute(mat)
  matsums <- colSums(mat)
  shapevec <- rowMeans(mat[, rep(which.max(matsums), 2)])
  shapevec <- shapevec/max(shapevec)
  mat <- antsGetNeighborhoodMatrix(img, mask, rad, boundary.condition = "image")
  mat <- antsrimpute(mat)
  shapecor <- cor(mat, shapevec)
  if (!missing(scfun))
    shapecor <- scfun(shapecor)
  featurei <- makeImage(mask, shapecor)
  return(featurei)
}
