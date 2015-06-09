#' Simple projectImageAlongAxis function.
#'
#' Will turn an N-D image into an N-1-D image by summation (0), max-intensity
#' (1) or min-intensity (2) projection along an orthogonal axis.
#'
#'
#' @param imageND input image
#' @param referenceImageNDminus1 down-dimensional image to help define physical #' space
#' @param projtype projection type 0, 1 or 2
#' @param axis should be less than image dimension and greater than or equal to
#' 0
#' @return image of n-dimensions-1 is output
#' @author Avants BB
#' @examples
#'
#' img<-makeImage(c(5,5,5,5),rnorm(5^4))
#' img3d<-makeImage(c(5,5,5),rnorm(5^3))
#' mask4dproj<-projectImageAlongAxis( img, img3d, axis=3, projtype=0 )
#'
#'
#' @export projectImageAlongAxis
projectImageAlongAxis <- function(imageND,
  referenceImageNDminus1, projtype = 0,
  axis = NA) {
  if (nargs() == 0) {
    print("Usage:  x_projected<-projectImageAlongAxis.Rd( x, xdownreference ) ")
    return(1)
  }
  if (is.na(axis))
    axis <- (imageND@dimension - 1)
  if (axis >= imageND@dimension)
    axis <- (imageND@dimension - 1)
  downimg <- antsImageClone(referenceImageNDminus1)
  downimg = iMath(imageND,"Project",axis,projtype)
  return(downimg)
}
