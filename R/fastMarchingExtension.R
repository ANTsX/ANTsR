#' @name fastMarchingExtension
#' @title Fast Marching Extension filter will extend an intensity or label image
#' from a known region into the unknown region along a geodesic path
#' @description This in an implementation of ITK's FastMarchingExtensionImageFilter - From http://www.itk.org/Doxygen/html/classitk_1_1FastMarchingExtensionImageFilter.html - Fast marching can be used to extend auxiliary variables smoothly from the zero level set. Starting from an initial position on the front, this class simultaneously calculate the signed distance and extend a set of auxiliary values. Implemenation of this class is based on Chapter 11 of "Level Set Methods and Fast Marching Methods", J.A. Sethian, Cambridge Press, Second edition, 1999.
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
