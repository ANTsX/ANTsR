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
#' @param x matrix-based smoothing
#' @param e restrict deformation boolean
#' @param verbose boolean
#' @param ... anything else, see KK help in ANTs
#' @return thickness antsImage
#' @author Shrinidhi KL, Avants BB
#' @examples
#'
#' img<-antsImageRead( getANTsRData("r16") ,2)
#' img<-resampleImage(img,c(64,64),1,0)
#' mask<-getMask( img )
#' segs<-kmeansSegmentation( img, k=3, kmask = mask)
#' thk<-kellyKapowski( s=segs$segmentation, g=segs$probabilityimages[[2]],
#'   w=segs$probabilityimages[[3]],its=45,r=0.5,m=1 )
#'
#' @export kellyKapowski
kellyKapowski <- function( s = NA, g = NA, w = NA,
   its = 50, r = 0.025,
   m = 1.5,  x = TRUE,
   e = FALSE, verbose = FALSE, ...) {
  if (missing(s) | missing(g) | missing(w) |
    is.na(s) | is.na(g) | is.na(w)) {
    print("Input error - check params & usage")
    return(NULL)
  }
  if (class(s)[1] == "antsImage") {
    s <- antsImageClone(s, "unsigned int")
  }
  # kellyKapowski( d=3, s=simg, g=gimg,w=wimg,c=10,r=0.5,m=1,o=oimg )
  d=s@dimension
  outimg=antsImageClone(g)
  itsstring = paste0( "[",its,",0,6]" )
  kkargs <- list(d = d, s = s, g = g, w = w,
    c = itsstring, r = r, m = m, e = as.numeric( e ), x = as.numeric( x ),
    o = outimg, v = as.numeric( verbose ),
    ...)
  temp<-.Call( "KellyKapowski", .int_antsProcessArguments(c(kkargs)),
    PACKAGE = "ANTsR" )
  return(outimg)
}
