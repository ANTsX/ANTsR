#' createJacobianDeterminantImage
#'
#' Compute the jacobian determinant from a transformation file
#'
#' @param domainImg image that defines transformation domain
#' @param tx deformation transformation file name
#' @param doLog return the log jacobian
#' @return jacobianImage
#' @author BB Avants
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' mi<-antsImageRead( getANTsRData("r64") ,2)
#' fi<-resampleImage(fi,c(128,128),1,0)
#' mi<-resampleImage(mi,c(128,128),1,0)
#' mytx<-antsRegistration(fixed=fi , moving=mi, typeofTransform = c("SyN") )
#' jac<-createJacobianDeterminantImage(fi,mytx$fwdtransforms[[1]],1)
#' # plot(jac)
#' @export createJacobianDeterminantImage
createJacobianDeterminantImage <- function( domainImg, tx, doLog = 0) {
  dim<-domainImg@dimension
  args <- list(dim, tx, doLog)
  dimg <- antsImageClone( domainImg, "double" )
  args2 <- list(dim, tx, dimg, doLog, 1)
  k <- .int_antsProcessArguments(args2)
  retval <- (.Call("CreateJacobianDeterminantImage", k, PACKAGE = "ANTsR"))
  jimg <- antsImageClone(args2[[3]], "float")
  return(jimg)
}
