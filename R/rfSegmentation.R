#' A rfSegmentation function.
#'
#' Image segmentation via random forests.
#'
#' @param labelimg input antsImage labelimage --- assume non-zero entries
#' create a mask
#' @param featureimages input list of antsImage feature images - length n means
#' n predictors in rf
#' @param ntrees number of rf trees
#' @param verbose boolean
#' @return list of n-probability images is output where n is number of classes
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' if ( usePkg("randomForest") ) {
#' img<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask( img )
#' segs<-kmeansSegmentation( img, k=3, kmask = mask)
#' fimgs<-lappend( img, segs$probabilityimages )
#' rfsegs<-rfSegmentation( segs$segmentation, fimgs , verbose=TRUE )
#' plot( rfsegs$segmentation )
#' # now use in atropos w/priors
#' segs2<-Atropos( a = img, m = '[0.2,1x1]',
#'   c = '[5,0]',  i = rfsegs$probabilityimages, x = mask)
#'  }
#'
#' @export rfSegmentation
rfSegmentation <- function(labelimg, featureimages,
  ntrees = 100, verbose = FALSE) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentation( x, x2 ) ")
    return(1)
  }
  haverf<-usePkg("randomForest")
  if ( !haverf ) {
    print("need randomForest package for this function")
    return(NA)
  }
  mask <- antsImageClone(labelimg)
  mask <- getMask(mask)
  labels <- as.factor(labelimg[mask == 1])
  fmat <- t(imageListToMatrix(featureimages, mask))
  mydf <- data.frame(labels = labels, fmat)
  myrf <- randomForest(labels ~ ., data = mydf, ntree = ntrees, type = "classification",
    importance = TRUE, na.action = na.omit, do.trace = verbose)
  if (verbose)
    print(myrf)
  probabilityimages <- predict(myrf, type = "prob")
  probabilityimages <- matrixToImages(t(probabilityimages), mask)
  segs <- antsImageClone(mask)
  segs[mask == 1] <- predict(myrf)
  myout <- list(segmentation = segs, probabilityimages = probabilityimages, rfModel = myrf)
  return(myout)
}
