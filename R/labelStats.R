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
#'
#' @export
labelStats <- function(image, labelImage){
  if(!is.antsImage(image) | !is.antsImage(labelImage))
    stop("Inputs must be antsImages.")
  if (!all(dim(image) == dim(labelImage)))
    stop("Images must be the same size.")
  image.float <- antsImageClone(image, "float")
  labelImage.int <- antsImageClone(labelImage, "unsigned int")
  df <- .Call("labelStats", image.float, labelImage.int, PACKAGE = "ANTsR")
  df=df[ order(df$LabelValue) , ]
  df
}
