#' @name basicInPaint
#'
#' @title Inpaints missing imaging data from boundary data
#'
#' @description  Smooths data along the boundary into the missing region.

#' @param img antsImage to be approximated / painted
#' @param paintMask painting mask with values 1 or
#' values 1 and 2 - if there is a 2 then it will learn
#' from label 1 to paint label 2.  should cover the brain.
#' @param speedimage - larger means faster/better
#' @param its - iterations of graddescent
#' @param gparam - graddescent param e.g. 0.05
#' @return inpainted image
#' @author Brian B. Avants
#' @keywords inpainting template
#' @examples
#' set.seed(123)
#' fi <- abs(replicate(100, rnorm(100)))
#' fi[1:10, ] <- fi[, 1:10] <- fi[91:100, ] <- fi[, 91:100] <- 0
#' mask <- fi
#' mask[mask > 0] <- 1
#' mask2 <- mask
#' mask2[11:20, 11:20] <- 2
#' mask <- as.antsImage(mask2)
#' fi <- as.antsImage(fi)
#' fi <- smoothImage(fi, 3)
#' \dontrun{
#' painted <- basicInPaint(fi, mask)
#' }
#' @export
basicInPaint <- function(img, paintMask, speedimage = NULL, its = 0, gparam = 0.05) {
  if (nargs() == 0) {
    print(args(basicInPaint))
    return(1)
  }
  inpainted <- antsImageClone(img)
  paintMaskUse <- antsImageClone(paintMask)
  temp <- antsImageClone(paintMask)
  temp[temp == 1] <- 0
  temp[temp == 2] <- 1
  temp <- iMath(temp, "MD", 1)
  paintMaskUse[temp == 1 & paintMaskUse == 1] <- 2
  healthymask <- antsImageClone(paintMaskUse)
  healthymask[paintMaskUse == 2] <- 0
  if (is.null(speedimage)) {
    speedimage <- antsImageClone(img)
    upit <- mean(img[paintMaskUse == 2])
    speedimage[paintMaskUse == 2] <- speedimage[paintMaskUse == 2] + upit
  } else {
    speedimage <- check_ants(speedimage)
  }
  inpainted <- fastMarchingExtension(speedimage, healthymask, img)
  outimg <- antsImageClone(img)
  outimg[paintMaskUse == 2] <- inpainted[paintMaskUse == 2]
  if (its > 0) {
    w2 <- (1 - gparam)
    sval <- min(antsGetSpacing(img))
    for (i in 1:its) {
      soutimg <- smoothImage(outimg, sval)
      v1 <- outimg[paintMaskUse == 2] * w2
      v2 <- soutimg[paintMaskUse == 2] * gparam
      outimg[paintMaskUse == 2] <- (v1 + v2)
    }
  }
  return(outimg)
}
