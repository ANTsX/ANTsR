#' Read file info from image header
#'
#' Read file info from image header
#'
#'
#' @param filename name of image file to scan for info

#' @author Duda JT
#' @examples
#'
#' \dontrun{
#' t = antsImageHeaderInfo( "testimage.nii.gz")
#' }
#'
#' @return outputs a list containing:
#' \itemize{
#'   \item{pixelclass: }{Type of pixel (scalar, vector, etc).}
#'   \item{pixeltype: }{Type of pixel values (int, float, etc).}
#'   \item{nDimensions: }{Number of image dimensions.}
#'   \item{nComponents: }{Number of pixel dimensions.}
#'   \item{dimensions: }{Size of image dimensions.}
#'   \item{spacing: }{Pixel resolution.}
#'   \item{origin: }{Spatial origin of image}
#'   \item{pixelclass: }{Spatial directions of image axes.}
#' }
#' @export antsImageHeaderInfo

antsImageHeaderInfo <- function( filename )
{

  if ( !file.exists(filename) )
  {
    stop("File does not exist")
  }

  ret =  .Call("antsImageHeaderInfo", filename, PACKAGE = "ANTsR")
  return(ret)

}
