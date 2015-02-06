#' Compute cortical thickness using the DiReCT algorithm.
#'
#' Diffeomorphic registration-based cortical thickness based on probabilistic
#' segmentation of an image.  This is an optimization algorithm.
#'
#'
#' @param s segmentation image
#' @param g gray matter probability image
#' @param w white matter probability image
#' @param its convergence params - controls iterations
#' @param r gradient descent update parameter
#' @param m gradient field smoothing parameter
#' @param ... anything else, see KK help in ANTs
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL, Avants BB
#' @examples
#'
#' img<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask( img )
#' segs<-kmeansSegmentation( img, k=3, kmask = mask)
#' kellyKapowski( s=segs$segmentation, g=segs$probabilityimages[[2]],
#'   w=segs$probabilityimages[[3]],its=45,r=0.5,m=1 )
#'
#' @export kellyKapowski
kellyKapowski <- function( s = NA, g = NA, w = NA,
   its = 50, r = 0.025,
   m = 1.5, outimg = outimg, ...) {
  if (missing(s) | missing(g) | missing(w) | missing(its) | missing(r) |
    missing(m)  ) {
    print("Input error - check params & usage")
    return(NULL)
  }
  if (class(s)[1] == "antsImage") {
    s <- antsImageClone(s, "unsigned int")
  }
  # kellyKapowski( d=3, s=simg, g=gimg,w=wimg,c=10,r=0.5,m=1,o=oimg )
  d=s@dimension
  outimg=antsImageClone(g)
  kkargs <- list(d = d, s = s, g = g, w = w, c = its, r = 0.5, m = 1,
    o = outimg)
  temp<-.Call( "kellyKapowski", .int_antsProcessArguments(c(kkargs)),
    PACKAGE = "ANTsR" )
  return(outimg)
}
