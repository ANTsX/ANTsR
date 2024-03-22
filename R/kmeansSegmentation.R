#' k means image segmentation.
#'
#' k means image segmentation that is a wrapper around atropos
#'
#' @param img input image
#' @param k integer number of classes
#' @param kmask segment inside this mask
#' @param mrf smoothness, higher is smoother
#' @param verbose boolean
#' @param ... additional arguments to pass to \code{\link{atropos}}
#' @return segmentation and probability images
#' @note This function will likely give different results on multiple runs.

#' @author Brian B. Avants
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' orig = antsImageClone(fi)
#' fi<-n3BiasFieldCorrection(fi,4)
#' seg<-kmeansSegmentation( fi, 3 )
#' seg2<-kmeansSegmentation( fi, 3 )
#' arr1 = as.array(seg$segmentation)
#' arr2 = as.array(seg2$segmentation)
#' testthat::expect_equal(arr1, arr2)
#'
#' set.seed(2)
#' # set below for slower but numerically repeatable results
#' # these should be set in .Renviron not by sys calls
#' # Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1 )
#' orig <-antsImageRead( getANTsRData("r16") ,2)
#' seg<-kmeansSegmentation( orig, 3 , use_random_seed = TRUE)
#' seg2<-kmeansSegmentation( orig, 3, use_random_seed = TRUE )
#' arr1 = as.array(seg$segmentation)
#' arr2 = as.array(seg2$segmentation)
#' tab = table(arr1, arr2)
#' tab
#' identical(arr1, arr2)
#' seg<-kmeansSegmentation( orig, 3, use_random_seed = FALSE )
#' seg2<-kmeansSegmentation( orig, 3, use_random_seed = FALSE )
#' arr1 = as.array(seg$segmentation)
#' arr2 = as.array(seg2$segmentation)
#' tab = table(arr1, arr2)
#' tab
#' @export kmeansSegmentation
kmeansSegmentation <- function(img, k, kmask = NULL, mrf = 0.1, verbose=FALSE,
                               ...) {
  img = check_ants(img)
  dim <- img@dimension
  kmimg = iMath(img, "Normalize")
  if (is.null(kmask)) {
    kmask <- getMask(kmimg, 0.01, 1, cleanup = 2)
  } else {
    kmask = check_ants(kmask)
  }
  kmask = iMath(kmask, "FillHoles") %>% thresholdImage(1,2)
  nhood <- paste(rep(1, dim), collapse = "x")
  mrf <- paste("[", mrf, ",", nhood, "]", sep = "")
  kmimg <- atropos( a = img, m = mrf, c = "[5,0]",
    i = paste("kmeans[",k, "]", sep = ""),
    verbose = verbose,
    x = kmask,
    ...)

  kmimg$segmentation <- antsImageClone(kmimg$segmentation, img@pixeltype)
  return(kmimg)
}
