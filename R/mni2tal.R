#' Brett's mni2tal
#' 
#' mni2tal for converting from ch2/mni space to tal - very approximate.
#' 
#' see
#' http://bioimagesuite.yale.edu/mni2tal/501_95733_More%20Accurate%20Talairach%20Coordinates%20SLIDES.pdf
#' 
#' http://imaging.mrc-cbu.cam.ac.uk/imaging/MniTalairach
#' 
#' This is a standard approach but it's not very accurate.
#' 
#' @param x Point in mni152 space.
#' @return The output point is in approximate Talairach space.
#' @author Matthew Brett, adapted for ANTsR by B Avants
#' @keywords Talairach
#' @examples
#' 
#' \dontrun{
#'   mni2tal( c(10,12,14) )
#' }
#' 
#' @export mni2tal
mni2tal <- function(xin = 0) {
  if (nargs() == 0 | length(xin) != 3) {
    print("Usage:  talPoint<-mni2tal( mniPoint ) ")
    print("mni2tal for converting from ch2/mni space to tal - very approximate")
    return(1)
  }
  x <- xin
  # The input image is in RAS coordinates but we use ITK which returns LPS
  # coordinates.  So we need to flip the coordinates such that L => R and P => A to
  # get RAS (MNI) coordinates
  x[1] <- x[1] * (-1)  # flip X
  x[2] <- x[2] * (-1)  # flip Y 
  xout <- x
  if (x[3] >= 0) {
    xout[1] <- x[1] * 0.99
    xout[2] <- x[2] * 0.9688 + 0.046 * x[3]
    xout[3] <- x[2] * (-0.0485) + 0.9189 * x[3]
  }
  if (x[3] < 0) {
    xout[1] <- x[1] * 0.99
    xout[2] <- x[2] * 0.9688 + 0.042 * x[3]
    xout[3] <- x[2] * (-0.0485) + 0.839 * x[3]
  }
  return(xout)
} 
