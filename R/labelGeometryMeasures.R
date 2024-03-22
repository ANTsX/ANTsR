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
labelGeometryMeasures <- function( labelImage, intensityImage=NULL ) {
  labelImage = check_ants(labelImage)
  if ( missing( intensityImage ) | 
       is.null(intensityImage)) {
    intensityImage <- labelImage
  }
  outcsv<-tempfile( fileext='.csv' )
  veccer<-list( labelImage@dimension, labelImage, intensityImage, outcsv )
  pp <- ANTsRCore::LabelGeometryMeasures(.int_antsProcessArguments(veccer) )
  pp = read.csv( outcsv )
  pp$Label = sort( unique( labelImage[ labelImage > 0 ] ) )
  names( pp )[2] = "VolumeInMillimeters"
  spc = prod( antsGetSpacing( labelImage ) )
  pp$VolumeInMillimeters = pp$VolumeInMillimeters * spc
  return( pp )
}
