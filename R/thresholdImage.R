#' Threshold Image
#'
#' converts a scalar image into a binary image by thresholding operations
#'
#' @usage thresholdImage(inimg, lothresh, hithresh, inval=1, outval=0)
#' @param inimg Input image to operate on
#' @param lothresh Lower edge of threshold window
#' @param hithresh Higher edge of threshold window
#' @param inval Output value for image voxels in between \code{lothresh} and \code{hithresh}
#' @param outval Output value for image voxels lower than \code{lothresh} or higher than \code{hithresh}
#' @return antsImage
#' @author Shrinidhi KL
#' @examples
#' set.seed(1234)
#' img <- makeImage(c(5,5), rnorm(25)+0.5)
#' imgt<-thresholdImage( img, 0.5, Inf )
#' testthat::expect_equal(sum(imgt), 9)
#' imgt<-thresholdImage( img > 0.5, 0.1, Inf )
#' testthat::expect_equal(sum(imgt), 9)
#' @export thresholdImage
thresholdImage <- function(inimg,
  lothresh, hithresh, inval=1, outval=0) {
  inimg = check_ants(inimg)
  inimg = antsImageClone(inimg, out_pixeltype = "float")
  dim <- inimg@dimension
  outimg <- antsImageClone( inimg )
  args <- list(dim, inimg, outimg, lothresh, hithresh, inval, outval)
  temp <- ANTsRCore::ThresholdImage(.int_antsProcessArguments(args))
  return(outimg)
}



#' Integrate velocity field
#'
#' Utility function to integrate a velocity field and create a deformation field.
#'
#' @param referenceImage defines the image domain
#' @param velocityFieldFileName the velocity field exists on disk.
#' @param deformationFieldFileName the deformation field output file name.
#' @param lowerTime the starting time, usually zero for forward transformation
#' and one for the inverse transformation.
#' @param upperTime the ending time, usually one for forward transformation
#' and zero for the inverse transformation.
#' @param deltaTime the integration time step
#' @return NULL
#' @author Avants BB
#' @examples
#' \dontrun{
#' set.seed(1234)
#' fi <- ( ri(1) )
#' mi <- ( ri(2) )
#' mytx2 <- antsRegistration(fixed=fi, mi, typeofTransform = "TV[4]" )
#' integrateVelocityField( fi, mytx2$velocityfield,  "/tmp/def.nii.gz" )
#' qq=antsApplyTransforms( fi, mi, mytx2$fwdtransforms )
#' pp=antsApplyTransforms( fi, mi, "/tmp/def.nii.gz" )
#' antsImageMutualInformation( fi, mi )
#' antsImageMutualInformation( fi, qq )
#' antsImageMutualInformation( fi, pp )
#' }
#' @export integrateVelocityField
integrateVelocityField <- function(
  referenceImage,
  velocityFieldFileName,
  deformationFieldFileName,
  lowerTime = 0.0,
  upperTime = 1.0,
  deltaTime = 0.01
) {
  referenceImage = check_ants(referenceImage)
  temp <- ANTsRCore::ANTSIntegrateVelocityField(referenceImage, velocityFieldFileName, deformationFieldFileName,
     lowerTime, upperTime, deltaTime)
}




#' Integrate vector field
#'
#' Utility function to integrate a vector field and create a deformation field.
#'
#' @param referenceImage defines the image domain
#' @param velocityFieldFileName the velocity field exists on disk.
#' @param deformationFieldFileName the deformation field output file name.
#' @param lowerTime the starting time, usually zero for forward transformation
#' and one for the inverse transformation.
#' @param upperTime the ending time, usually one for forward transformation
#' and zero for the inverse transformation.
#' @param deltaTime the integration time step
#' @return NULL
#' @author Avants BB
#' @examples
#' \dontrun{
#' set.seed(1234)
#' fi <- ( ri(1) )
#' mi <- ( ri(2) )
#' mytx2 <- antsRegistration(fixed=fi, mi, typeofTransform = "SyN" )
#' integrateVectorField( fi, mytx2$velocityfield,  "/tmp/def.nii.gz" )
#' }
#' @export integrateVectorField
integrateVectorField <- function(
  referenceImage,
  vectorFieldFileName,
  deformationFieldFileName,
  lowerTime = 0.0,
  upperTime = 1.0,
  deltaTime = 0.01
) {
  referenceImage = check_ants(referenceImage)
  veccer = antsImageRead( vectorFieldFileName )
  antsImageWrite( veccerdplus1, deformationFieldFileName )
  temp <- ANTsRCore::ANTSIntegrateVectorField(referenceImage, deformationFieldFileName, deformationFieldFileName,
     lowerTime, upperTime, deltaTime)
}
