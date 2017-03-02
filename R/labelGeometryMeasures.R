#' labelGeometryMeasures
#'
#' Wrapper for the ANTs funtion labelGeometryMeasures
#'
#' @param labelImage on which to compute geometry
#' @param intensityImage optional
#' @return none
#' @author Avants BB, Tustison NJ
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' seg<-kmeansSegmentation( fi, 3 )$segmentation
#' geom<-labelGeometryMeasures(seg,fi)
#'
#'
#' @export labelGeometryMeasures
labelGeometryMeasures <- function( labelImage, intensityImage=NA ) {
  if ( missing( intensityImage )) intensityImage <- labelImage
  outcsv<-tempfile( fileext='.csv' )
  veccer<-list( labelImage@dimension, labelImage, intensityImage, outcsv )
  pp <- .Call("LabelGeometryMeasures", .int_antsProcessArguments(veccer) )
  pp = read.csv( outcsv )
  pp$Label = sort( unique( labelImage[ labelImage > 0 ] ) )
  return( pp )
}
