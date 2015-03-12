#' Uses example images to inpaint or approximate an existing image.
#'
#' Employs a robust regression approach to learn the relationship between a
#' sample image and a list of images that are mapped to the same space as the
#' sample image.  The regression uses data from an image neighborhood.
#'
#'
#' @param img antsImage to be approximated / painted
#' @param paintMask painting mask with values 1 or values 1 and 2 - if there is
#' a 2 then it will learn from label 1 to paint label 2.  should cover the
#' brain.
#' @param imageList a list containing antsImages
#' @param featureRadius - radius of image neighborhood e.g. 2
#' @param scaleInpaintIntensity - brighter or darker painted voxels, default of
#' 0 sets this parameter automatically
#' @param sharpen - sharpen the approximated image
#' @param feather - value (e.g. 1) that helps feather the mask for smooth
#' blending
#' @param predalgorithm - string svm or lm
#' @param debug - TRUE or FALSE
#' @return inpainted image
#' @author Brian B. Avants
#' @keywords inpainting template
#' @examples
#'
#' set.seed(123)
#' fi<-abs(replicate(100, rnorm(100)))
#' fi[1:10,]<-fi[,1:10]<-fi[91:100,]<-fi[,91:100]<-0
#' mask<-fi
#' mask[ mask > 0 ]<-1
#' mask2<-mask
#' mask2[11:20,11:20]<-2
#' mask<-as.antsImage( mask , "float" )
#' fi<-as.antsImage( fi , "float" )
#' fi<-smoothImage(fi,3)
#' mo<-as.antsImage( replicate(100, rnorm(100)) , "float" )
#' mo2<-as.antsImage( replicate(100, rnorm(100)) , "float" )
#' ilist<-list(mo,mo2)
#' painted<-exemplarInpainting(fi,mask,ilist)
#' mask2<-as.antsImage( mask2 , "float" )
#' painted2<-exemplarInpainting(fi,mask2,ilist)
#' # just use 1 image, so no regression is performed
#' painted3<-exemplarInpainting(fi,mask2, list(ilist[[1]]))
#'
#' @export exemplarInpainting
exemplarInpainting <- function(img, paintMask, imageList,
  featureRadius = 2, scaleInpaintIntensity = 0,
  sharpen = FALSE, feather = 1, predalgorithm = "lm", debug = FALSE) {
  havesvm<-FALSE
  if ( predalgorithm == 'svm' )
    {
    havesvm<-usePkg("e1071")
    }
  mask <- antsImageClone(paintMask)
  mask[paintMask != 1] <- 0  # dont use the lesion
  inpaintLesion <- FALSE
  nlist <- length(imageList)
  if (max(paintMask) == 2)
    inpaintLesion <- TRUE
  if (debug)
    print(paste(inpaintLesion, "inpaintLesion"))
  if (inpaintLesion) {
    lmask <- antsImageClone(paintMask)
    lmask[paintMask != 2] <- 0  # use the lesion
    lmask[paintMask == 2] <- 1  # use the lesion
    fmask <- antsImageClone(paintMask)
    fmask[paintMask < 0.5] <- 0  # full mask
    fmask[paintMask >= 0.5] <- 1  #
    featherMask<-smoothImage(fmask, feather)
    featherMask2 <- antsImageClone(lmask)
    featherMask2[featherMask2 >= 0] <- 1
    featherMask2[featherMask > 0] <- 1 - featherMask[featherMask > 0]
  }
  if (debug)
    print(paste("got er done"))
  if (nlist > 1) {
    targetvoxels <- img[mask == 1]
    radius <- rep(featureRadius, img@dimension)
    nmat <- matrix()
    lmat <- matrix()
    fmat <- matrix()
    ct <- 1
    for (i in imageList) {
      mat <- (getNeighborhoodInMask(i, mask, radius, boundary.condition = "image"))
      if (all(dim(nmat) == 1))
        nmat <- t(mat) else nmat <- cbind(nmat, t(mat))
      if (inpaintLesion) {
        mat <- (getNeighborhoodInMask(i, lmask, radius, boundary.condition = "image"))
        if (all(dim(lmat) == 1))
          lmat <- t(mat) else lmat <- cbind(lmat, t(mat))
        mat <- (getNeighborhoodInMask(i, fmask, radius, boundary.condition = "image"))
        if (all(dim(fmat) == 1))
          fmat <- t(mat) else fmat <- cbind(fmat, t(mat))
      }
      if (debug)
        print(paste("built predictors", ct))
      ct <- ct + 1
    }
    nmatdf <- data.frame(nmat)
    if (nrow(nmatdf) != length(targetvoxels)) {
      print("nrow(nmatdf) != length(targetvoxels)")
      return(mask)
    }
    if (debug)
      print("run lm")
    if (predalgorithm == "svm" & havesvm ) {
      mdl <- e1071::svm(targetvoxels ~ ., data = nmatdf)
    } else mdl <- lm(targetvoxels ~ ., data = nmatdf)
    if (inpaintLesion == FALSE) {
      pvox <- predict(mdl, type = "response")
      predimg <- makeImage(mask, pvox)
      return(predimg)
    }
    # otherwise predict from full mat and feather-combine
    lmatdf <- data.frame(fmat)
    lesvox <- predict(mdl, newdata = lmatdf)
    predimg <- makeImage(fmask, lesvox)
  } else {
    if (debug)
      print(paste("just basic replacement"))
    predimg <- antsImageClone(imageList[[1]])
    predvec <- predimg[paintMask == 1]
    imgvec <- img[paintMask == 1]
    mydf <- data.frame(vox = predimg[paintMask == 1])
    if (predalgorithm == "svm" & havesvm ) {
      mdl <- e1071::svm(imgvec ~ vox, data = mydf)
    } else mdl <- lm(imgvec ~ vox, data = mydf)
    mydf <- data.frame(vox = predimg[fmask == 1])
    predvec2 <- predict(mdl, newdata = mydf)
    if (debug)
      print(summary(mdl))
    predimg[fmask == 1] <- predvec2
    if (debug)
      print(paste(mean(imgvec), mean(predvec2)))
  }
  if (sharpen)
    imageMath(img@dimension, predimg, "Sharpen", predimg)
  # now make two vectors - one for the lesion and one for the original image
  if (debug)
    print(dim(predimg))
  vec1 <- img[fmask == 1] * featherMask2[fmask == 1]
  vec2 <- predimg[fmask == 1] * featherMask[fmask == 1]
  imgvec <- img[paintMask == 1]
  predvec <- predimg[paintMask == 1]
  if (scaleInpaintIntensity == 0) {
    sci <- mean(imgvec) * 0.5 + sd(imgvec)
    scp <- mean(predvec) * 0.5 + sd(predvec)
    scaleInpaintIntensity <- sci/scp
    if (debug)
      print(paste("scaleInpaintIntensity", scaleInpaintIntensity))
  }
  predimg <- antsImageClone(img)  # copy image - then replace in full mask
  predimg[fmask == 1] <- vec2 * scaleInpaintIntensity + vec1
  return(predimg)
  # for bayesian regression - amazingly fast!  W2 = invcov.shrink(t(nmat), 0.1)
}
