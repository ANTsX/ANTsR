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
  if ( class( tx ) == "antsImage" ) {
    txuse = tempfile( fileext = c(".nii.gz") )
    antsImageWrite( tx, txuse )
    } else txuse = tx
  args <- list(dim, txuse, doLog)
  dimg <- antsImageClone( domainImg, "double" )
  args2 <- list(dim, txuse, dimg, doLog, 0)
  k <- .int_antsProcessArguments(args2)
  retval <- (.Call("CreateJacobianDeterminantImage", k, PACKAGE = "ANTsR"))
  jimg <- antsImageClone(args2[[3]], "float")
  return(jimg)
}
