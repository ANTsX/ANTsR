#' hausdorffDistance
#'
#' Wrapper for the ITK class HausdorffDistanceImageFilter.  More documentaiton
#' available here:
#'
#' \url{https://itk.org/Doxygen/html/classitk_1_1HausdorffDistanceImageFilter.html}
#'
#' @param inputImage1 image 1.
#' @param inputImage2 image 2.
#' @return data frame with overlap measures
#' @author Tustison NJ
#' @examples
#'
#' sourceImage <- antsImageRead(getANTsRData("r16"), 2)
#' sourceSegmentation <- kmeansSegmentation(sourceImage, 3)$segmentation
#' referenceImage <- antsImageRead(getANTsRData("r16"), 2)
#' referenceSegmentation <- kmeansSegmentation(referenceImage, 3)$segmentation
#' hausdorff <- hausdorffDistance(sourceImage, referenceImage)
#'
#' @export hausdorffDistance
hausdorffDistance <- function(inputImage1, inputImage2) {
  inputImage1 <- check_ants(inputImage1)
  inputImage2 <- check_ants(inputImage2)

  hausdorff <- ANTsRCore::HausdorffDistanceR(
    inputImage1, inputImage2
  )

  return(hausdorff)
}
