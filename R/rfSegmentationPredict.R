#' A rfSegmentationPredict function.
#'
#' Predict image segmentation via random forests.
#'
#' @param rfSegmentationModel input rf model
#' @param featureimages input list of antsImage feature images
#' @param mask antsImage mask
#' @param verbose bool
#' @return segmentation is output
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' if ( usePkg('randomForest') ) {
#' img<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask( img )
#' mask2<-getMask( img )
#' mask<-iMath(mask,'ME',25)
#' mask2[ mask == 1 ]<-0
#' segs<-kmeansSegmentation( img, k=3, kmask = mask)
#' fimgs<-list( img )
#' rfsegs<-rfSegmentation( segs$segmentation, fimgs , ntrees=100 )
#' rfseg2<-rfSegmentationPredict(  rfsegs$rfModel , fimgs , mask2 )
#' plot( rfseg2 )
#' \dontrun{
#'    img<-antsImageRead( getANTsRData("r16") ,2)
#'    img2<-antsImageRead( getANTsRData("r64") ,2)
#'    mask<-getMask( img )
#'    mask2<-getMask( img2 )
#'    segs<-kmeansSegmentation( img, k=3, kmask = mask)
#'    nimg<-iMath(img,"Normalize")
#'    fimgs<-list( nimg )
#'    rfsegs<-rfSegmentation( segs$segmentation, fimgs , ntrees=100 )
#'    mytx<-antsRegistration(fixed=img , moving=img2 ,
#'             typeofTransform = c('SyN') )
#'    fimgs2<-list( iMath(mytx$warpedmovout,"Normalize") )
#'    rfseg2<-rfSegmentationPredict(  rfsegs$rfModel , fimgs2 , mask )
#'    plot( rfseg2 )
#'    }
#' }
#'
#' @export rfSegmentationPredict
rfSegmentationPredict <- function(rfSegmentationModel, featureimages,
   mask, verbose = FALSE) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentationPredict( rfSegmentationModel, featureimages , mask ) ")
    return(1)
  }
  fmat <- t(imageListToMatrix(featureimages, mask))
  mydf <- data.frame(fmat)
  segs <- antsImageClone(mask)
  segs[mask == 1] <- predict(rfSegmentationModel, newdata = fmat )
  return(segs)
}
