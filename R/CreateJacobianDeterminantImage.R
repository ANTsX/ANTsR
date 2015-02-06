#' CreateJacobianDeterminantImage
#'
#' Compute the jacobian determinant from a transformation file
#'
#' @param dim Number of dimensions of the input tx
#' @param tx deformation transformation file name
#' @param doLog return the log jacobian
#' @return jacobianImage
#' @author BB Avants
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' fi<-resampleImage(fi,c(128,128),1,0)
#' mi<-antsImageRead( getANTsRData('r64') ,2)
#' mi<-resampleImage(mi,c(128,128),1,0)
#' mytx<-antsRegistration(fixed=fi , moving=mi, typeofTransform = c('SyN') )
#' jac<-CreateJacobianDeterminantImage(2,mytx$fwdtransforms[[1]],1)
#' # plot(jac)
#' @export ImageMath
CreateJacobianDeterminantImage <- function(dim, tx, doLog = 0) {
  args <- list(dim, tx, doLog)
  img <- antsImageRead(tx, dim)
  dimg <- antsImageClone(img, "double")
  args2 <- list(dim, tx, dimg, doLog, 1)
  k <- .int_antsProcessArguments(args2)
  retval <- (.Call("CreateJacobianDeterminantImage", k, PACKAGE = "ANTsR"))
  jimg <- antsImageClone(args2[[3]], "float")
  return(jimg)
}
