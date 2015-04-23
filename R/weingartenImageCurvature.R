#' image curvature for 3D
#'
#' uses the weingarten map to estimate image mean or gaussian curvature
#'
#' @param image antsImage
#' @param sigma smoothing parameter
#' @param opt mean by default, otherwise gaussian or characterize
#' @return image
#' @author Brian B. Avants
#' @references Avants, B, J. Gee, and B. Avants.
#' Shape operator for differential image analysis
#' Information Processing in Medical Imaging, 2003.
#' @keywords curvature
#' @examples
#' img = makeImage( c(10,10,10) , rnorm( 1000 ) )
#' fik <- weingartenImageCurvature( img )
#' if ( abs( mean(fik) - 128 ) > 1 ) stop("weingartenImageCurvature failure")
#' \dontrun{
#' fi <- antsImageRead( getANTsRData( "mni" ) )
#' fik <- weingartenImageCurvature( fi )
#' }
#' @export weingartenImageCurvature
weingartenImageCurvature <- function( image, sigma=1.0, opt='mean' ) {
  if ( image@dimension != 3  ) {
    stop("input image must be 3D")
  }
  optnum=0
  if ( opt == 'gaussian' ) optnum=6
  if ( opt == 'characterize' ) optnum=5
  .Call("weingartenImageCurvature",
    image, sigma, optnum, PACKAGE = "ANTsR")
}
