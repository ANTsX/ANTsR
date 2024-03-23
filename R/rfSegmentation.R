#' A rfSegmentation function.
#'
#' Unsupervised image segmentation via random forests.  An example.
#'
#' @param featureMatrix input matrix of features matched to mask size
#' n predictors in rf
#' @param mask input antsImage mask
#' @param labelimg input antsImage labelimage, optional for supervised seg
#' @param ntrees number of rf trees
#' @param verbose boolean
#' @return list of n-probability images is output where n is number of classes
#' @author Tustison NJ, Avants BB
#' @seealso \code{\link{mrvnrfs}}
#' @examples
#' \dontrun{
#' if (usePkg("randomForest")) {
#'   img <- antsImageRead(getANTsRData("r16")) %>% iMath("Normalize")
#'   mask <- getMask(img)
#'   segs <- kmeansSegmentation(img, k = 3, kmask = mask)
#'   fmat0 <- t(antsrimpute(getNeighborhoodInMask(img, mask, c(2, 2))))
#'   fmat1 <- t(antsrimpute(getNeighborhoodInMask(
#'     segs$probabilityimages[[1]], mask, c(2, 2)
#'   )))
#'   fmat2 <- t(antsrimpute(getNeighborhoodInMask(
#'     segs$probabilityimages[[2]], mask, c(2, 2)
#'   )))
#'   fmat3 <- t(antsrimpute(getNeighborhoodInMask(
#'     segs$probabilityimages[[3]], mask, c(2, 2)
#'   )))
#'   fmat <- cbind(fmat0, fmat1, fmat2, fmat3)
#'   # produces proximity between all voxel pairs
#'   rfsegs <- rfSegmentation(fmat, verbose = FALSE)
#'   lrr <- lowrankRowMatrix(rfsegs, 10, faster = TRUE)
#'   nv <- eanatSelect(
#'     inmat = lrr, mask = mask, selectorScale = 1.2, cthresh = 50,
#'     verbose = T, smooth = 1
#'   )
#'   ee <- eanatDef(lrr, mask,
#'     nvecs = nv, smooth = 0., cthresh = 50,
#'     its = 2, verbose = TRUE
#'   )
#'   eseg <- eigSeg(mask, ee)
#'   plot(img, eseg)
#' }
#' }
#' @export rfSegmentation
rfSegmentation <- function(
    featureMatrix,
    mask, labelimg = NULL,
    ntrees = 100, verbose = FALSE) {
  if (nargs() == 0) {
    print("Usage:  probs<-rfSegmentation( x, x2 ) ")
    return(1)
  }
  haverf <- usePkg("randomForest")
  if (!haverf) {
    print("need randomForest package for this function")
    return(NA)
  }
  mask <- check_ants(mask)
  haveLabels <- !is.null(labelimg)
  if (haveLabels) {
    labelimg <- check_ants(labelimg)
    labels <- as.factor(labelimg[mask == 1])
    mydf <- data.frame(labels = labels, featureMatrix)
    myrf <- randomForest::randomForest(
      y = labels, x = featureMatrix, ntree = ntrees, type = "classification",
      importance = TRUE, na.action = na.omit, do.trace = verbose
    )
  } else {
    myrf <- randomForest::randomForest(
      x = featureMatrix, ntree = ntrees,
      type = "unsupervised",
      importance = TRUE, na.action = na.omit, do.trace = verbose
    )
    return(myrf$proximity)
  }
  if (verbose) print(myrf)
  probabilityimages <- predict(myrf, type = "prob")
  probabilityimages <- matrixToImages(t(probabilityimages), mask)
  segs <- antsImageClone(mask)
  segs[mask == 1] <- predict(myrf)
  myout <- list(segmentation = segs, probabilityimages = probabilityimages, rfModel = myrf)
  return(myout)
}

#' A rfSegmentationPredict function.
#'
#' Predict image segmentation via random forests.
#'
#' @param rfSegmentationModel input rf model
#' @param featureMatrix input feature matrix
#' @param mask antsImage mask
#' @param verbose bool
#' @return segmentation is output
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' if (usePkg("randomForest")) {
#'   img <- antsImageRead(getANTsRData("r16"))
#'   mask <- getMask(img)
#'   mask2 <- getMask(img)
#'   mask[129:255, 1:255] <- 0
#'   mask2[2:128, 1:255] <- 0
#'   segs <- kmeansSegmentation(img, k = 3, kmask = mask)
#'   fmat <- t(antsrimpute(getNeighborhoodInMask(img, mask, c(2, 2))))
#'   rfsegs <- rfSegmentation(fmat, mask, segs$segmentation, ntrees = 100)
#'   fmat2 <- t(antsrimpute(getNeighborhoodInMask(img, mask2, c(2, 2))))
#'   rfseg2 <- rfSegmentationPredict(rfsegs$rfModel, fmat2, mask2)
#' }
#'
#' @export rfSegmentationPredict
rfSegmentationPredict <- function(
    rfSegmentationModel, featureMatrix,
    mask, verbose = FALSE) {
  segs <- antsImageClone(mask)
  segs[mask == 1] <- predict(rfSegmentationModel, newdata = featureMatrix)
  return(segs)
}
