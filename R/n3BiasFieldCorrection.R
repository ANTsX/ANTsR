#' N3 bias field correction
#'
#' Perform N3 bias field correction on the given image
#'
#' @param img antsImage to correct
#' @param downsampleFactor integer e.g. 4 downsample by a factor of 4
#' @param ... not used
#' @return antsImage
#' @author Shrinidhi KL
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' n3img<-n3BiasFieldCorrection(  img, 1 )
#'
#' @export n3BiasFieldCorrection
n3BiasFieldCorrection <- function( img, downsampleFactor, ... ) {
  img = check_ants(img)
  outimg <- antsImageClone(img)
  args <- list(img@dimension, img, outimg, downsampleFactor)
  pp <- ANTsRCore::N3BiasFieldCorrection(.int_antsProcessArguments(args))
  return(outimg)
}

#' N3 Bias field correction
#'
#' Perform N3 bias field correction on the given image
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
#' typically integer less than 4
#' @param convergence List of:  \code{iters}, maximum number of
#' iterations and \code{tol}, the convergence tolerance.
#' Default tolerance is 1e-7 in ANTsR/ANTsPy but 0.0 in ANTs.
#' @param splineParam Parameter controlling number of control points in spline.
#' Either single value, indicating how many control points, or vector
#' with one entry per dimension of image, indicating the spacing in each direction.
#' Default is a mesh size of 1 per dimension.
#' @param numberOfFittingLevels Parameter controlling number of fitting levels.
#' @param weightMask antsImage of weight mask
#' @param returnBiasField bool, return the field instead of the corrected image.
#' @param verbose enables verbose output.
#' @return bias corrected image or bias field
#' @author Avants BB, Tustison NJ
#' @examples
#'  dims = c(50, 50)
#'  img<-makeImage(imagesize = dims, rnorm(prod(dims)) )
#'  n3img<-n3BiasFieldCorrection2(img)
#'  n3img<-n3BiasFieldCorrection2(img, mask = img > 0)
#'  testthat::expect_error(n3BiasFieldCorrection2(img, weightMask = "somepath"))
#'  testthat::expect_error(n3BiasFieldCorrection2(img, splineParam = rep(200, 3)))
#'  # n3img<-n3BiasFieldCorrection2(img, splineParam = c(200, 20)) # long running
#'
#'  rm(img); gc()
#'  rm(n3img); gc()
#' fname = getANTsRData("r16")
#' in_img = antsImageRead(fname)
#' n3 = n3BiasFieldCorrection2(in_img)
#' rm(n3); gc()
#' mask = in_img > 0
#' mask2 = antsImageClone(mask, out_pixeltype = "float")
#' # fails
#' mask
#' sum(mask)
#' \dontrun{
#' n3 = n3BiasFieldCorrection2(in_img, mask = mask, verbose = TRUE)
#' # fails
#' n3 = n3BiasFieldCorrection2(in_img, mask = mask2)
#' }
#' @export n3BiasFieldCorrection2
n3BiasFieldCorrection2 <- function( img,
                                    mask,
                                    rescaleIntensities = FALSE,
                                    shrinkFactor = 4,
                                    convergence = list(iters = 50, tol = 1e-7),
                                    splineParam = 200,
                                    numberOfFittingLevels = 4,
                                    weightMask = NULL,
                                    returnBiasField = FALSE,
                                    verbose = FALSE )
{
  img = check_ants(img)
  if ( var( img ) == 0 ) stop("Input image has no variation.")
  if ( ! missing( mask ) ) {
    mask = check_ants(mask)
    error_not_antsImage(mask, "mask")
  }

  # if mask was character - silent change below - bad
  # if (!is.antsImage(mask)) {
  #   mask <- getMask(img)
  # }
  N3_CONVERGENCE_1 <-
    paste(
      "[",
      convergence$iters,
      ",",
      sprintf("%.10f", convergence$tol),
      "]",
      sep = ""
    )
  N3_SHRINK_FACTOR_1 <- paste(shrinkFactor)
  if (length(splineParam) == 1) {
    N3_BSPLINE_PARAMS <- paste("[", splineParam, ",", numberOfFittingLevels, "]", sep = "")
  } else if (length(splineParam) == img@dimension) {
    N3_BSPLINE_PARAMS <-
      paste("[", paste(splineParam, collapse = "x"), numberOfFittingLevels, "]", sep = "")
  }  else {
    stop("Length of splineParam must either be 1 or dimensionality of image.")
  }

  if (!is.null(weightMask)) {
    weightMask <- check_ants(weightMask)
    if (!is.antsImage(weightMask)) {
      stop("Weight Image must be an antsImage")
    }
  }

  outimg <- antsImageClone(img)*0
  biasimg <- antsImageClone(img)*0
  ptr1=antsrGetPointerName(outimg)
  ptr2=antsrGetPointerName(biasimg)
  args =
    list(d = outimg@dimension,
         i = img)
  if ( ! missing( weightMask ) ) args$w = weightMask
  args$s = N3_SHRINK_FACTOR_1
  args$c = N3_CONVERGENCE_1
  args$b = N3_BSPLINE_PARAMS
  args$r = as.numeric( rescaleIntensities )
  if ( ! missing( mask ) ) args$x = mask
  args$o = paste0("[",ptr1,",",ptr2,"]")
  args$v = as.numeric(verbose > 0)

  .helpn3BiasFieldCorrection2(args)
  if ( returnBiasField ) return( biasimg )
  return(outimg)
}
.helpn3BiasFieldCorrection2 <- function(...) {
  ANTsRCore::N3BiasFieldCorrection(.int_antsProcessArguments(c(...)))
}
