#' reflectImage
#'
#' reflects an image along its axis
#'
#' @param img1 input object, an antsImage
#' @param axis which dimension to reflect across, numbered from 0 to imageDimension-1
#' @param tx transformation type to estimate after reflection
#' @param metric similarity metric for image registration.  see \code{antsRegistration}.
#' @param verbose print diagnostic messages, passed to
#' \code{\link{antsRegistration}} and \code{\link{antsApplyTransforms}}
#' @param ... Additional options to pass to \code{\link{antsRegistration}}
#'
#' @author BB Avants
#' @seealso \code{\link{antsRegistration}}
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' axis = 2
#' asym<-reflectImage( fi, axis, "Affine" )$warpedmovout
#' asym<-asym-fi
#'
#' @export reflectImage
reflectImage<-function(img1, axis=NA, tx=NA, metric="mattes",
verbose = TRUE,
... ) {
  if ( is.na(axis) ) {
    axis=( img1@dimension - 1 )
  }
  if ( axis > img1@dimension | axis < 0 ) {
    axis=(img1@dimension-1)
  }

  rflct<-tempfile(fileext = ".mat")
  catchout = .Call( "reflectionMatrix", img1, axis, rflct, package="ANTsR")

  if ( ! all(is.na(tx) ))
  {
    rfi <- antsRegistration( img1, img1, typeofTransform = tx,
                             synMetric = metric,
                             outprefix = tempfile(),
                             initialTransform = rflct,
                             verbose = verbose,
                             ...)
    return( rfi )
  } else {
    return( antsApplyTransforms( img1, img1, rflct, verbose = verbose)  )
  }
}
