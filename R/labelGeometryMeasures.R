#' labelGeometryMeasures
#'
#' Wrapper for the ANTs funtion labelGeometryMeasures
#'
#' @param labelImage on which to compute geometry
#' @param intensityImage optional
#' @return none
#' @author Avants BB, Tustison NJ
#' @examples
#' fi <- antsImageRead(getANTsRData("r16"), 2)
#' seg <- kmeansSegmentation(fi, 3)$segmentation
#' geom <- labelGeometryMeasures(seg, fi)
#'
#' @export labelGeometryMeasures
labelGeometryMeasures <- function(labelImage, intensityImage = NULL) {
  labelImage <- check_ants(labelImage)
  maxLabel <- 2^32 - 1 # maximum value for uint32, as used in ANTs function
  if (any(labelImage < 0) || any(labelImage > maxLabel)) {
    stop("labelImage contains values outside the valid range [0, ", maxLabel, "].")
  }
  labelImage <- antsImageClone(labelImage, "unsigned int")
  if (missing(intensityImage) |
    is.null(intensityImage)) {
    intensityImage <- labelImage
  }
  outcsv <- tempfile(fileext = ".csv")
  veccer <- list(labelImage@dimension, labelImage, intensityImage, outcsv)
  pp <- ANTsRCore::LabelGeometryMeasures(.int_antsProcessArguments(veccer))
  pp <- read.csv(outcsv)
  return(pp)
}

