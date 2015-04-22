#' split a multichannel antsImage
#'
#' split a multichannel antsImage into a list of scalar antsImages
#'
#' @param image a multichannel antsImage to split
#' @return list of scalar antsImages
#' @author Duda, JT
#' @examples
#'  r <- floor( seq(1:(64*64)) / (64*64) * 255 )
#'  dim(r) <- c(64,64)
#'  r <- as.antsImage(r)
#'  g <- r*0
#'  b <- r*0
#'  rgbImage = mergeChannels( list(r,g,b) )
#'  imgList = splitChannels( rgbImage )
#'
#' @export splitChannels
splitChannels <- function(image) {

  if ( !is.antsImage( image ) ) {
    stop( "input must be an 'antsImage'" )
    }
  if ( !(image@components > 1) ) {
    stop( "input must have more than 1 components")
  }

  img = .Call("splitChannels", image, package="ANTsR")
  return(img)
}
