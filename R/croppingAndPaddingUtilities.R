#' Crop the center of an image.
#'
#' @param image Input ANTs image
#' @param cropSize width, height, depth (if 3-D), and time (if 4-D) of
#' the cropped image.
#'
#' @return a cropped image
#' @author Tustison NJ
#' @examples
#'
#' library( ANTsR )
#'
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' croppedImage <- cropImageCenter( image, c( 64, 64 ) )
#'
#' @export
cropImageCenter <- function( image, cropSize )
{
  imageSize <- dim( image )

  if( length( imageSize ) != length( cropSize ) )
    {
    stop( "cropSize does not match image size." )
    }

  if( any( cropSize > imageSize ) )
    {
    stop( "A cropSize dimension is larger than imageSize." )
    }

  startIndex <- floor( 0.5 * ( imageSize - cropSize ) ) + 1
  endIndex <- startIndex + cropSize - 1

  croppedImage <- cropIndices( image, startIndex, endIndex )

  return( croppedImage )
}

#' Pad an image based on a factor.
#'
#' Pad image of size \code{(x, y, z)} to \code{(x', y', z')} where
#' \code{(x', y', z')} is a divisible by a user-specified factor.
#'
#' @param image Input ANTs image
#' @param factor padding factor.  Can be an integer or vector of size
#' equal to the image dimensionality.
#'
#' @return a padded image
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' library( ANTsR )
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' image <- cropImage( image )
#' paddedImage <- padImageByFactor( image, 4 )
#'
#' @export
padImageByFactor <- function( image, factor )
  {

  factorVector <- factor
  if( length( factor ) == 1 )
    {
    factorVector <- rep( factor, image@dimension )
    }
  if( length( factorVector ) != image@dimension )
    {
    stop( "factor must be scalar or the length of the image dimension." )
    }

  imageSize <- dim( image )
  deltaSize <- imageSize %% factorVector

  paddedSize <- imageSize
  for( i in seq.int( length( paddedSize ) ) )
    {
    if( deltaSize[i] > 0 )
      {
      paddedSize[i] <- imageSize[i] - deltaSize[i] + factorVector[i]
      }
    }

  paddedImage <- padOrCropImageToSize( image, paddedSize )

  return( paddedImage )
  }

#' Pad or crop image to a specified size
#'
#' @param image Input ANTs image
#' @param size size of the output image.
#'
#' @return a padded/cropped image
#' @author Tustison NJ
#' @examples
#'
#' library( ANTsR )
#'
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' paddedImage <- padOrCropImageToSize( image, c( 333, 333 ) )
#'
#' @export
padOrCropImageToSize <- function( image, size )
  {
  imageSize <- dim( image )
  delta <- imageSize - size

  if( any( delta < 0 ) )
    {
    padSize <- ceiling( 0.5 * abs( min( delta ) ) )
    image <- iMath( image, "PadImage", padSize )
    }
  croppedImage <- cropImageCenter( image, size )
  return( croppedImage )
  }
