#' N4 bias field correction
#'
#' Perform N4 bias field correction on the given image
#'
#' @param img input antsImage
#' @param mask input mask, if one is not passed one will be made
#' @param rescaleIntensities At each iteration, a new intensity mapping is
#' calculated and applied but there is nothing which constrains the new
#' intensity range to be within certain values. The result is that the
#' range can "drift" from the original at each iteration. This option
#' rescales to the [min,max] range of the original image intensities within
#' the user-specified mask. A mask is required to perform rescaling.  Default
#' is FALSE in ANTsR/ANTsPy but TRUE in ANTs.
#' @param shrinkFactor Shrink factor for multi-resolution correction,
#' typically integer less than 4.
#' @param convergence List of:  \code{iters}, vector of maximum number of
#' iterations for each shrinkage factor, and \code{tol}, the convergence tolerance.
#' Default tolerance is 1e-7 in ANTsR/ANTsPy but 0.0 in ANTs.
#' @param splineParam Parameter controlling number of control points in spline.
#' Either single value, indicating how many control points, or vector
#' with one entry per dimension of image, indicating the spacing in each direction.
#' Default in ANTsR/ANTsPy is 200 mm per mesh element in each dimension.  The ANTs
#' default is a mesh size of 1 per dimension.
#' @param weightMask antsImage of weight mask
#' @param returnBiasField bool, return the field instead of the corrected image.
#' @param verbose enables verbose output.
#' @return bias corrected image or bias field
#' @author BB Avants
#' @examples
#' dims <- c(50, 50)
#' img <- makeImage(imagesize = dims, rnorm(prod(dims)))
#' n4img <- n4BiasFieldCorrection(img)
#' n4img <- n4BiasFieldCorrection(img, mask = img > 0)
#' testthat::expect_error(n4BiasFieldCorrection(img, weightMask = "somepath"))
#' testthat::expect_error(n4BiasFieldCorrection(img, splineParam = rep(200, 3)))
#' n4img <- n4BiasFieldCorrection(img, splineParam = c(200, 20))
#'
#' rm(img)
#' gc()
#' rm(n4img)
#' gc()
#' fname <- getANTsRData("r16")
#' in_img <- antsImageRead(fname)
#' n4 <- n4BiasFieldCorrection(in_img)
#' rm(n4)
#' gc()
#' mask <- in_img > 0
#' mask2 <- antsImageClone(mask, out_pixeltype = "float")
#' # fails
#' mask
#' sum(mask)
#' \dontrun{
#' n4 <- n4BiasFieldCorrection(in_img, mask = mask, verbose = TRUE)
#' # fails
#' n4 <- n4BiasFieldCorrection(in_img, mask = mask2)
#' }
#' @export
n4BiasFieldCorrection <- function(img,
                                  mask,
                                  rescaleIntensities = FALSE,
                                  shrinkFactor = 4,
                                  convergence = list(iters = c(50, 50, 50, 50), tol = 1e-7),
                                  splineParam = 200,
                                  returnBiasField = FALSE,
                                  verbose = FALSE,
                                  weightMask = NULL) {
  img <- check_ants(img)
  if (var(img) == 0) stop("Input image has no variation.")
  if (!missing(mask)) {
    mask <- check_ants(mask)
    error_not_antsImage(mask, "mask")
  }
  # if mask was character - silent change below - bad
  # if (!is.antsImage(mask)) {
  #   mask <- getMask(img)
  # }
  N4_CONVERGENCE_1 <-
    paste(
      "[",
      paste(convergence$iters, collapse = "x"),
      ",",
      sprintf("%.10f", convergence$tol),
      "]",
      sep = ""
    )
  N4_SHRINK_FACTOR_1 <- paste(shrinkFactor)
  if (length(splineParam) == 1) {
    N4_BSPLINE_PARAMS <- paste("[", splineParam, "]", sep = "")
  } else if (length(splineParam) == img@dimension) {
    N4_BSPLINE_PARAMS <-
      paste("[", paste(splineParam, collapse = "x"), "]", sep = "")
  } else {
    stop("Length of splineParam must either be 1 or dimensionality of image.")
  }

  if (!is.null(weightMask)) {
    weightMask <- check_ants(weightMask)
    if (!is.antsImage(weightMask)) {
      stop("Weight Image must be an antsImage")
    }
  }

  outimg <- antsImageClone(img) * 0
  biasimg <- antsImageClone(img) * 0
  ptr1 <- antsrGetPointerName(outimg)
  ptr2 <- antsrGetPointerName(biasimg)
  args <-
    list(
      d = outimg@dimension,
      i = img
    )
  if (!missing(weightMask)) args$w <- weightMask
  args$s <- N4_SHRINK_FACTOR_1
  args$c <- N4_CONVERGENCE_1
  args$b <- N4_BSPLINE_PARAMS
  args$r <- as.numeric(rescaleIntensities)
  if (!missing(mask)) args$x <- mask
  args$o <- paste0("[", ptr1, ",", ptr2, "]")
  args$v <- as.numeric(verbose > 0)

  .helpn4BiasFieldCorrection(args)
  if (returnBiasField) {
    return(biasimg)
  }
  return(outimg)
}
.helpn4BiasFieldCorrection <- function(...) {
  ANTsRCore::N4BiasFieldCorrection(.int_antsProcessArguments(c(...)))
}
