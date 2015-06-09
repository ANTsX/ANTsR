#' @name fastMarchingExtension
#' @title Fast Marching Extension
#' @description Fast marching extension
#' @param speedImage
#' @param labelImage
#' @param valueImage
#' @return antsImage
#' @author Duda, JT
#' @export fastMarchingExtension
fastMarchingExtension <- function(speedImage, labelImage, valueImage) {
  outimg <- .Call("fastMarchingExtension",
    speedImage, labelImage, valueImage,
    PACKAGE = "ANTsR")
  return(outimg)
}
