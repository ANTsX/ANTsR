#' @title labelStats
#' @description Get label statistics from image.
#' @usage labelStats(image, labelImage)
#' @param image Image to calculate statistics from.
#' @param labelImage Label image.
#' @return Data frame with one row per label (including zero) and columns :
#'   LabelValues, Mean, Min, Max, Variance, Count, Volume
#' @examples
#'
#' img <- antsImageRead( getANTsRData("r16") , 2 )
#' img <- resampleImage( img, c(64,64), 1, 0 )
#' mask <- getMask(img)
#' segs1 = kmeansSegmentation( img, 3 )
#' labelStats(img, segs1$segmentation)
#' testthat::expect_error(labelStats(img, as.array(segs1$segmentation)))
#' testthat::expect_error(labelStats(img, as.array(segs1$segmentation)))
#' sub_seg = segs1$segmentation
#' sub_seg = as.antsImage(sub_seg[1:62, 1:62], reference = sub_seg)
#' testthat::expect_error(labelStats(img, sub_seg))
#'
#' @export
labelStats <- function(image, labelImage) {
  image = check_ants(image)
  labelImage = check_ants(labelImage)
  
  if (!is.antsImage(image) | !is.antsImage(labelImage))
    stop("Inputs must be antsImages.")
  if (!all(dim(image) == dim(labelImage)))
    stop("Images must be the same size.")
  image.float <- antsImageClone(image, "float")
  labelImage.int <- antsImageClone(labelImage, "unsigned int")
  df <- ANTsRCore::labelStats(image.float, labelImage.int)
  df = df[ order(df$LabelValue) , ]
  df
}
