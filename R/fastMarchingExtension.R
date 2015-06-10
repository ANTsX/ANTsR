#' @name fastMarchingExtension
#' @title Fast Marching Extension filter will extend an intensity or label image
#' from a known region into the unknown region along a geodesic path
#' @description Fast marching extension
#' @param speedImage defines the cost or distance function
#' @param labelImage defines the known (value 1) and unknown (value 2) regions
#' @param valueImage these values are extended into the unknown regions
#' @return antsImage
#' @author Duda, JT
#' @export fastMarchingExtension
fastMarchingExtension <- function(speedImage, labelImage, valueImage) {
  outimg <- .Call("fastMarchingExtension",
    speedImage, labelImage, valueImage,
    PACKAGE = "ANTsR")
  return(outimg)
}
