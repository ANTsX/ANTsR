#' CreateJacobianDeterminantImage
#'
#' Compute the jacobian determinant from a transformation file
#'
#' @param imageDimension2|3 Number of dimensions of the input tx
#' @param deformationField transformation file name
#' @param outputImage result image, cloned from a fixed image
#' @param doLog return the log jacobian
#' @return 0 -- Success\cr 1 -- Failure
#' @author BB Avants
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' mi<-antsImageRead( getANTsRData('r64') ,2)
#' mytx<-antsRegistration(fixed=fi , moving=mi, typeofTransform = c("SyN") )
#' jac<-antsImageClone(fi)
#' CreateJacobianDeterminantImage(2,mytx$fwdtransforms[[1]],jac,1)
#' plot(jac)
#' @export ImageMath
CreateJacobianDeterminantImage <- function(...) {
  args<-list(...)
  if ( length(args) <= 1 ) args<-list("")
  if (length(args)==4) args[[5]]<-1
  .Call("CreateJacobianDeterminantImage", as.character(c(...)))
}
