#' Compute cortical thickness using the DiReCT algorithm.
#' 
#' Diffeomorphic registration-based cortical thickness based on probabilistic
#' segmentation of an image.  This is an optimization algorithm.
#' 
#' 
#' @param imageDimension2|3|4 Number of dimensions of the input image
#' @param inputImage Input image to operate on
#' @param s segmentation image
#' @param g gray matter probability image
#' @param w white matter probability image
#' @param c convergence params - first controls iterations
#' @param r gradient descent update parameter
#' @param m gradient field smoothing parameter
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL, Avants BB
#' @examples
#' 
#' \dontrun{
#' KellyKapowski( d=3, s=simg, g=gimg,w=wimg,c=45,r=0.5,m=1,o=oimg )
#' }
#' 
#' @export KellyKapowski
KellyKapowski <- function(d = NA, s = NA, g = NA, w = NA, c = "[45,0.0,10]", r = 0.025, 
  m = 1.5, outimg = outimg, ...) {
  if (missing(d) | missing(s) | missing(g) | missing(w) | missing(c) | missing(r) | 
    missing(m) | missing(outimg)) {
    print("Input error - check params & usage")
    return(NULL)
  }
  if (class(s)[1] == "antsImage") {
    s <- antsImageClone(s, "unsigned int")
  }
  # KellyKapowski( d=3, s=simg, g=gimg,w=wimg,c=10,r=0.5,m=1,o=oimg )
  kkargs <- list(d = d, s = s, g = g, w = w, c = 10, r = 0.5, m = 1, outimg = outimg)
  .Call("KellyKapowski", int_antsProcessArguments(c(kkargs)), PACKAGE = "ANTsR")
} 
