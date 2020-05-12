#' addNoiseToImage
#'
#' Add noise to image using additive Guassian, salt-and-pepper,
#' shot, or speckle noise.
#'
#' @param image input image
#' @param noiseModel either "additivegaussian", "saltandpepper", "shot",
#' or "speckle".
#' @param noiseParameters vector defining the noise models.  
#' \code{additivegaussian}: (mean, standardDeviation),
#' \code{saltandpepper}: (probability, saltValue, pepperValue),
#' \code{shot}: (scale),
#' \code{speckle}: (standardDeviation),
#' @return noise corrupted image.
#'
#' @author NJ Tustison
#'
#' @examples
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' noiseImage <- addNoiseToImage( image, "additivegaussian", c( 0, 1 ) )
#' noiseImage <- addNoiseToImage( image, "saltandpepper", c( 0.1, 0, 100 ) )
#' noiseImage <- addNoiseToImage( image, "shot", c( 1.0 ) )
#' noiseImage <- addNoiseToImage( image, "speckle", c( 1.0 ) )
#'
#' @export addNoiseToImage

addNoiseToImage <- function(
  image,
  noiseModel = c( "additivegaussian", "saltandpepper", "shot", "speckle" ),
  noiseParameters
  ) {

  whichNoiseModel <- -1
  noiseModel <- match.arg( noiseModel )
  if( noiseModel == 'additivegaussian' )
    {
    whichNoiseModel <- 0L
    if( length( noiseParameters ) != 2 )
      {
      stop( "Error:  incorrect number of parameters." )
      }
    } else if( noiseModel == "saltandpepper" ) {
    whichNoiseModel <- 1L
    if( length( noiseParameters ) != 3 )
      {
      stop( "Error:  incorrect number of parameters." )
      }
    } else if( noiseModel == "shot" ) {
    whichNoiseModel <- 2L
    if( length( noiseParameters ) != 1 )
      {
      stop( "Error:  incorrect number of parameters." )
      }
    } else if( noiseModel == "speckle" ) {
    whichNoiseModel <- 3L
    if( length( noiseParameters ) != 1 )
      {
      stop( "Error:  incorrect number of parameters." )
      }
    } else {
    stop( "Error:  unrecognized noise model." )
    }  

  outputImage <- .Call( "addNoiseToImageR",
    antsImageClone( image ),
    whichNoiseModel,
    as.numeric( noiseParameters ),
    PACKAGE = "ANTsR" )
  return( outputImage )
}
