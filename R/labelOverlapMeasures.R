#' labelOverlapMeasures
#'
#' Wrapper for the ANTs funtion LabelOverlapMeasures.  More documentaiton
#' available here:
#'
#' \url{https://www.insight-journal.org/browse/publication/707}
#'
#' @param sourceLabelImage label image for source image.
#' @param targetLabelImage label image for target/reference image.
#' @return data frame with overlap measures
#' @author Avants BB, Tustison NJ
#' @examples
#'
#' sourceImage <- antsImageRead( getANTsRData( "r16" ), 2 )
#' sourceSegmentation <- kmeansSegmentation( sourceImage, 3 )$segmentation
#' referenceImage <- antsImageRead( getANTsRData( "r16" ), 2 )
#' referenceSegmentation <- kmeansSegmentation( referenceImage, 3 )$segmentation
#' overlap <- labelOverlapMeasures( sourceSegmentation, referenceSegmentation )
#'
#' @export labelOverlapMeasures
labelOverlapMeasures <- function( sourceLabelImage, targetLabelImage ) 
  {
  sourceLabelImage <- check_ants( sourceLabelImage )
  targetLabelImage <- check_ants( targetLabelImage )

  overlapMeasures <- .Call( "labelOverlapMeasuresR", 
    sourceLabelImage, targetLabelImage, PACKAGE = "ANTsRCore" )
  overlapMeasures[1, 1] <- "All"

  return( overlapMeasures )
}
